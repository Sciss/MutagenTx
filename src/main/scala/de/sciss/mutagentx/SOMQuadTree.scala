package de.sciss.mutagentx

import de.sciss.file._
import de.sciss.lucre.data.DeterministicSkipOctree
import de.sciss.lucre.data.gui.SkipQuadtreeView
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D, IntSquare}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.InMemory
import de.sciss.mutagentx.SOMGenerator._
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.{kollflitz, numbers}

import scala.annotation.tailrec
import scala.concurrent.blocking
import scala.swing.event.MousePressed
import scala.swing.{Component, MainFrame, Swing}

object SOMQuadTree extends App {
  def numCoeff = 13

  def extent        : Int             = 16384
  def gridStep      : Int             = 32 // 16

  run(args.headOption.getOrElse("betanovuss0"))

  import Algorithm.executionContext

  def run(name: String): Unit = {
    val latticeFut = Processor[Lattice[Node]]("lattice") { self =>
      val graphDB = SynthGraphDB.open(name)
      val nodes: Vec[Node] = blocking {
        import graphDB._
        system.step { implicit tx =>
          var res = Vector.newBuilder[Node]
          handle().iterator.foreach(_.iterator.foreach(n => res += n))
          res.result()
        }
      }
      graphDB.system.close()

      // cf. http://oogifu.blogspot.com/2014/07/self-organizing-map-in-scala.html

      implicit val rnd = new util.Random

      import kollflitz.RandomOps._
      import numbers.Implicits._

//      val featMin = Array(-1473.435, -2.223, -3.793, -4.728, -1.674, -1.988, -2.095, -2.003, -1.324, -1.997, -1.884, -3.046, -4.375)
//      val featMax = Array(  151.118, 57.481, 20.539, 10.134, 8.485,   6.312,  5.287,  3.876,  4.552,  2.771,  3.031,  3.047,  3.272)

      val featSpecMin = Array(-1560.6178,-21.7986,-4.6478,-5.2797,-3.6140,-4.1488,-3.9983,-3.6434,-2.4137,-2.2102,-2.5199,-4.4990,-6.6068)
      val featSpecMax = Array(176.5447,55.3165,20.5961,10.6913,8.8670,6.5897,5.7930,4.1103,4.9481,3.7107,4.0118,3.8707,3.4230)

      def normalize(w: Weight): Weight = {
        val fNew = w.spectral.zipWithIndex.map { case (v, i) => v.linlin(featSpecMin(i), featSpecMax(i), 0, 1) }
        w.copy(spectral = fNew)
      }

      // println("WARNING: TAKE 1000")
      val nodesN = nodes.map(n => n.copy(weight = normalize(n.weight))) // .take(1000)

      def rndWeight(): Weight = {
        val spectral = Array.fill(numCoeff)(rnd.nextDouble())
        val temporal = Array.fill(numCoeff)(rnd.nextDouble())
        Weight(spectral, temporal)
      }

      def weightDist(w1: Weight, w2: Weight): Double = {
        def norm(v: Array[Double]): Vec[Double] = (0 until v.length).map { i => v(i).linlin(featSpecMin(i), featSpecMax(i), 0, 1) }
        val w1n = norm(w1.spectral)
        val w2n = norm(w2.spectral)
        (w1n zip w2n).map { tup => (tup._1 - tup._2).squared } .sum.sqrt
      }

      def nodeDist(n1: AnyNode, n2: AnyNode): Double =
        math.sqrt((n1.coord.x - n2.coord.x).squared + (n1.coord.y - n2.coord.y).squared) // Euclidean distance

      println("WARNING: should scramble training set instead of using `choose`")
      val trainingSet   : Vec[Node]       = nodesN
      println("WARNING: NUM ITER = 1000")
      val numIterations : Int             = 1000 // trainingSet.size * 4 // XXX
      val mapRadius     : Double          = extent
      val timeConstant  : Double          = numIterations / math.log(mapRadius)

      def neighbourhoodRadius(iter: Double) = mapRadius * math.exp(-iter/timeConstant)

      def bmu(input: HasWeight, lattice: Lattice[HasWeight]): PlacedNode[HasWeight] =
        lattice.nodes.minBy(n => weightDist(input.weight, n.node.weight))

      def bmuNeighbours(radius: Double, bmu: AnyNode,
                        lattice: AnyLattice): (Vec[(AnyNode, Double)], Vec[(AnyNode, Double)]) =
        lattice.nodes.map(n => (n, nodeDist(n, bmu))).partition(_._2 <= radius)

      def learningRate(iter: Double): Double =
        0.072 * math.exp(-iter / numIterations) // decays over time

      def theta(d2bmu: Double, radius: Double): Double =
        math.exp(-d2bmu.squared / (2 * radius.squared)) // learning proportional to distance

      def adjust[W <: HasWeight](input: W, weight: Weight, learningRate: Double, theta: Double): input.Self = {
        def perform(iW: Double, nW: Double): Double =
          nW + learningRate * theta * (iW - nW)

        val spectralNew = (input.weight.spectral, weight.spectral).zipped.map(perform)
        val temporalNew = (input.weight.temporal, weight.temporal).zipped.map(perform)
        input.replaceWeight(Weight(spectralNew, temporalNew))
      }

      def nextLattice(iter: Int, lattice: AnyLattice): AnyLattice = {
        if (iter % 100 == 0) println(s"---- iter $iter")
        val randomInput   = trainingSet.choose()
        val bmuNode       = bmu(randomInput, lattice)
        val radius        = neighbourhoodRadius(iter)
        val (inNodes, outNodes) = bmuNeighbours(radius, bmuNode, lattice)
        val lRate         = learningRate(iter)
        val adjustedNodes = inNodes.par.map { case (n, nodeRadius) =>
          val tTheta  = theta(nodeRadius, radius)
          val nWeight = adjust(randomInput, n.node.weight, lRate, tTheta)
          PlacedNode(n.coord, nWeight)
        } .toIndexedSeq

        self.progress = (iter + 1).toDouble / numIterations
        self.checkAborted()

        new Lattice(/* lattice.size, */ adjustedNodes ++ outNodes.map(_._1))
      }

      def mkLattice(): AnyLattice = {
        val nodes = for {
          x <- -extent until extent by gridStep
          y <- -extent until extent by gridStep
        } yield {
            val c = Coord(x, y)
            val w = rndWeight()
            PlacedNode[HasWeight](c, w)
          }

        new Lattice(/* size, */ nodes)
      }

      @tailrec def run(in: AnyLattice, iter: Int): AnyLattice =
        if (iter >= numIterations) in else {
          val out = nextLattice(iter = iter, lattice = in)
          run(out, iter + 1)
        }

      println(s"--- num nodes = ${trainingSet.size}; num iterations = $numIterations")
      val lattice0 = mkLattice()
      val latticeN = run(lattice0, iter = 0)

      val res: Lattice[Node] = {
        // val nodes = latticeN.nodes.collect { case PlacedNode(c, n: Node) => PlacedNode(c, n) }
        val nodes = trainingSet.map { in =>
          val nearest = bmu(in, latticeN)
          PlacedNode(nearest.coord, in)
        }
        Lattice(/* size = latticeSize, */ nodes = nodes)
      }

      res
    }

    // futWeights.monitor(printResult = false)

    latticeFut.monitor()
    latticeFut.onFailure {
      case ex =>
        println("lattice generation failed:")
        ex.printStackTrace()
    }

    latticeFut.onSuccess { case lattice =>
      val ws = lattice.nodes
      println(s"Number of weights: ${ws.size}")

      /*
      import kollflitz.Ops._

      val perc = (0 until 13).map { i =>
        val all = ws.map { n =>
          n.node.weight.feature(i)
        }
        val sorted = all.sortedT
        (sorted.percentile(2), sorted.percentile(98))
      }
      println("Feature vector  2% percentiles:")
      println(perc.map(tup => f"${tup._1}%1.3f").mkString("[", ",", "]"))
      println("Feature vector 98% percentiles:")
      println(perc.map(tup => f"${tup._2}%1.3f").mkString("[", ",", "]"))
      */

      // import numbers.Implicits._
      // val extent  = ((lattice.size + 1) / 2).nextPowerOfTwo
      implicit val system  = InMemory()
      val quadH = system.step { implicit tx =>
        implicit object nodeSerializer extends Serializer[I#Tx, I#Acc, PlacedNode[Node]] {
          def read(in: DataInput, access: I#Acc)(implicit tx: I#Tx): PlacedNode[Node] = sys.error("not supported")
          def write(v: PlacedNode[Node], out: DataOutput): Unit = sys.error("not supported")
        }
        implicit val pointView = (n: PlacedNode[Node], tx: I#Tx) => n.coord
        val quad = DeterministicSkipOctree.empty[I, Dim, PlacedNode[Node]](IntSquare(extent, extent, extent))
        lattice.nodes.foreach(quad.add)
        tx.newHandle(quad)
      }

      Swing.onEDT(guiInit(quadH))
    }

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }
  }

  /*

Feature vector  2% percentiles:
[-1473.435,-2.223,-3.793,-4.728,-1.674,-1.988,-2.095,-2.003,-1.324,-1.997,-1.884,-3.046,-4.375]
Feature vector 98% percentiles:
[151.118,57.481,20.539,10.134,8.485,6.312,5.287,3.876,4.552,2.771,3.031,3.047,3.272]


   */

  def guiInit(quadH: stm.Source[I#Tx, DeterministicSkipOctree[I, Dim, PlacedNode[Node]]])(implicit system: I): Unit = {
    // val model = InteractiveSkipOctreePanel.makeModel2D(system)(())
    // new InteractiveSkipOctreePanel(model)
    val quadView = new SkipQuadtreeView[I, PlacedNode[Node]](quadH, system, _.coord)
    quadView.scale = 240.0 / extent
    val quadComp = Component.wrap(quadView)
    new MainFrame {
      contents = quadComp
      pack().centerOnScreen()
      open()
    }
    quadComp.listenTo(quadComp.mouse.clicks)
    val insets = quadView.getInsets
    quadComp.reactions += {
      case MousePressed(_, pt, mod, clicks, _) =>
        import numbers.Implicits._
        val x = ((pt.x - insets.left) / quadView.scale - extent + 0.5).toInt.clip(-extent, extent)
        val y = ((pt.y - insets.top ) / quadView.scale - extent + 0.5).toInt.clip(-extent, extent)

        val nodeOpt = system.step { implicit tx =>
          val q = quadH()
          q.nearestNeighborOption(IntPoint2D(x, y), IntDistanceMeasure2D.euclideanSq)
        }
        nodeOpt.foreach { node =>
          println(node)
        }
    }
  }
}
