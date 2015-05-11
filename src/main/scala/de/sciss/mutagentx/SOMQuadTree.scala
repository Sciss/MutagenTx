package de.sciss.mutagentx

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

  def extent        : Int             = 256 // 16384
  def gridStep      : Int             = 1 // 64 // 32 // 16

  run(args.headOption.getOrElse("betanovuss0"))

  import Algorithm.executionContext

  final case class Dist(node: AnyNode, radius: Double)

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

      val featSpecMin = Array(-1554.8430,-21.7765,-4.5185,-5.2909,-3.6100,-4.1124,-3.9966,-3.6367,-2.4137,-2.2400,-2.4790,-4.5608,-6.6133)
      val featSpecMax = Array(176.9208,55.4446,20.6956,10.6913,8.8621,6.5888,5.7932,4.1103,4.9481,3.7131,4.0103,3.8659,3.4231)
      val featTempMin = Array(0.3567,-3.2570,-2.8579,-2.1212,-1.9764,-1.8338,-2.3039,-1.8459,-1.6096,-1.5539,-1.3541,-1.4128,-1.4109)
      val featTempMax = Array(3100155267289.2250,3049041188044.5825,2937674475702.0060,2888555993728.8633,2603175515013.0580,2561710809328.4565,2572049306867.7030,2272804304747.4520,2409931779268.9175,2502593081103.6147,2758321940498.5825,2663814737416.1343,2637628956055.0970)


      def normalize(w: Weight): Unit = {
        w.spectral.zipWithIndex.foreach { case (v, i) =>
          val norm = v.linlin(featSpecMin(i), featSpecMax(i), 0, 1)
          w.spectral(i) = norm
        }
        w.temporal.zipWithIndex.foreach { case (v, i) =>
          val norm = v.linlin(featTempMin(i), featTempMax(i), 0, 1)
          w.temporal(i) = norm
        }
      }

      // println("WARNING: TAKE 1000")
      nodes.foreach(n => normalize(n.weight)) // .take(1000)

      def rndWeight(): Weight = {
        val spectral = Array.fill(numCoeff)(rnd.nextDouble())
        val temporal = Array.fill(numCoeff)(rnd.nextDouble())
        new Weight(spectral, temporal)
      }

      def weightDist(w1: Weight, w2: Weight): Double = {
        def norm(v: Array[Double], min: Array[Double], max: Array[Double]): Vec[Double] =
          (0 until v.length).map { i => v(i).linlin(min(i), max(i), 0, 1) }

        val w1sn = norm(w1.spectral, featSpecMin, featSpecMax)
        val w2sn = norm(w2.spectral, featSpecMin, featSpecMax)
        val spectDist = (w1sn zip w2sn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt

        val w1tn = norm(w1.temporal, featTempMin, featTempMax)
        val w2tn = norm(w2.temporal, featTempMin, featTempMax)
        val tempDist = (w1tn zip w2tn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt

        (spectDist + tempDist) / 2
      }

      def nodeDist(n1: AnyNode, n2: AnyNode): Double =
        math.sqrt((n1.coord.x - n2.coord.x).squared + (n1.coord.y - n2.coord.y).squared) // Euclidean distance

      val MAX_NODES = 20

      val trainingSet   : Array[Node]     = nodes.scramble().take(MAX_NODES).toArray
      // println("WARNING: NUM ITER = 1000")
      val numIterations : Int             = trainingSet.length // * 4
      val mapRadius     : Double          = extent
      val timeConstant  : Double          = numIterations / math.log(mapRadius)

      def neighbourhoodRadius(iter: Double) = mapRadius * math.exp(-iter / timeConstant)

      def mkLattice(): AnyLattice = {
        val nodes = for {
          x <- 0 until (extent << 1) by gridStep
          y <- 0 until (extent << 1) by gridStep
        } yield {
            val c = Coord(x, y)
            val w = rndWeight()
            new PlacedNode[HasWeight](c, w)
          }

        new Lattice(/* size, */ nodes.toArray)
      }

      val lattice = mkLattice()

      def bmu(input: HasWeight): PlacedNode[HasWeight] = {
        val nodes = lattice.nodes
        val iw = input.weight
        var i = 0
        var bestDist = Double.PositiveInfinity
        var bestNode: AnyNode = null
        while (i < nodes.length) {
          val n = nodes(i)
          val dist = weightDist(iw, n.node.weight)
          if (dist < bestDist) {
            bestDist = dist
            bestNode = n
          }
          i += 1
        }
        require(bestNode != null)
        bestNode
      }

      def bmuNeighbours(radius: Double, bmu: AnyNode,
                        lattice: AnyLattice): Iterator[Dist] =
        lattice.nodes.iterator.map(n => Dist(n, nodeDist(n, bmu))).filter(_.radius <= radius)

      def learningRate(iter: Double): Double =
        0.072 * math.exp(-iter / numIterations) // decays over time

      def theta(d2bmu: Double, radius: Double): Double =
        math.exp(-d2bmu.squared / (2 * radius.squared)) // learning proportional to distance

      def adjust[W <: HasWeight](input: W, weight: Weight, learningRate: Double, theta: Double): Unit = {
        @inline def perform(iW: Double, nW: Double): Double =
          nW + learningRate * theta * (iW - nW)

        // val spectralNew = (input.weight.spectral, weight.spectral).zipped.map(perform)
        // val temporalNew = (input.weight.temporal, weight.temporal).zipped.map(perform)
        // input.replaceWeight(Weight(spectralNew, temporalNew))

        @inline def performA(ia: Array[Double], wa: Array[Double]): Unit = {
          var i = 0
          while (i < ia.length) {
            wa(i) = perform(ia(i), wa(i))
            i += 1
          }
        }

        performA(input.weight.spectral, weight.spectral)
        performA(input.weight.temporal, weight.temporal)
      }

      def nextLattice(iter: Int): Unit = {
        if (iter % 100 == 0) println(s"---- iter $iter")
        val randomInput   = trainingSet(iter % trainingSet.length) // .choose()
        val bmuNode       = bmu(randomInput)
        val radius        = neighbourhoodRadius(iter)
        val inNodeIter    = bmuNeighbours(radius, bmuNode, lattice)
        val lRate         = learningRate(iter)
        val inNodeB = Vector.newBuilder[Dist]
        inNodeIter.foreach(inNodeB += _)
        inNodeB.result().par.foreach { dist =>
          val tTheta  = theta(dist.radius, radius)
          adjust(randomInput, dist.node.node.weight, lRate, tTheta)
        }

        self.progress = ((iter + 1).toDouble / numIterations) // * 0.5
        self.checkAborted()
      }

      @tailrec def run(iter: Int): Unit =
        if (iter < numIterations) {
          nextLattice(iter = iter)
          run(iter + 1)
        }

      println(s"--- num nodes = ${trainingSet.length}; num iterations = $numIterations")
      // val lattice0 = mkLattice()
      // val latticeN = run(lattice0, iter = 0)
      run(iter = 0)

      val res: Lattice[Node] = {
        // val nodes = latticeN.nodes.collect { case PlacedNode(c, n: Node) => PlacedNode(c, n) }
        val nodes = trainingSet.zipWithIndex.par.map { case (in, idx) =>
          val nearest = bmu(in)
          // self.progress = ((idx + 1).toDouble / 1000) * 0.5 + 0.5
          // self.checkAborted()
          new PlacedNode(nearest.coord, in)
        } .toArray
        new Lattice(/* size = latticeSize, */ nodes = nodes)
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
      println(s"Number of weights: ${ws.length}")

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
    // quadView.scale = 240.0 / extent
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
        val x = ((pt.x - insets.left) / quadView.scale + 0.5).toInt.clip(0, extent << 1)
        val y = ((pt.y - insets.top ) / quadView.scale + 0.5).toInt.clip(0, extent << 1)

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
