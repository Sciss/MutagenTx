package de.sciss.mutagentx

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import de.sciss.audiowidgets.Transport
import de.sciss.file._
import de.sciss.lucre.data.DeterministicSkipOctree
import de.sciss.lucre.data.gui.SkipQuadtreeView
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D, IntSpace, IntSquare}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mutagentx.SOMGenerator._
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.synth.{Synth, SynthDef, Server, ServerConnection}
import de.sciss.synth.proc.Durable
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.{kollflitz, numbers}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{TimeoutException, Await, Future, blocking}
import scala.swing.event.MousePressed
import scala.swing.{Button, BorderPanel, FlowPanel, Component, MainFrame, Swing}
import scala.util.Try

object SOMQuadTree extends App {
  def numCoeff  : Int   = 13

  def extent    : Int   = 256 // 16384
  def gridStep  : Int   = 1 // 64 // 32 // 16

  def MAX_NODES : Int   = 16384

  run(args.headOption.getOrElse("betanovuss0"))

  import Algorithm.executionContext

  // case class Coord(x: Int, y: Int)
  type Coord = IntPoint2D
  def  Coord(x: Int, y: Int) = IntPoint2D(x, y)

  class PlacedWeight(val coord: Coord, val weight: Weight)

  object PlacedNode {
    implicit object serializer extends ImmutableSerializer[PlacedNode] {
      private final val COOKIE = 0x506C6100 // "Pla\0"

      def write(pn: PlacedNode, out: DataOutput): Unit = {
        out.writeInt(COOKIE)
        IntPoint2D.serializer.write(pn.coord, out)
        Node      .serializer.write(pn.node , out)
      }

      def read(in: DataInput): PlacedNode = {
        val cookie = in.readInt()
        if (cookie != COOKIE) sys.error(s"Unexpected cookie, found ${cookie.toHexString} expected ${COOKIE.toHexString}")
        val coord = IntPoint2D.serializer.read(in)
        val node  = Node      .serializer.read(in)
        new PlacedNode(coord, node)
      }
    }
  }
  case class PlacedNode(coord: Coord, node: Node)

  class Lattice(val nodes: Array[PlacedWeight])

  final case class Dist(node: PlacedWeight, radius: Double)

  type Dim  = IntSpace.TwoDim

  object QuadGraphDB {
    type Tpe = DeterministicSkipOctree[D, Dim, PlacedNode]

    def open(name: String): QuadGraphDB = {
      val somDir = file("database") / s"${name}_som"
      implicit val dur  = Durable(BerkeleyDB.factory(somDir))
      implicit val pointView = (n: PlacedNode, tx: D#Tx) => n.coord
      implicit val octreeSer = DeterministicSkipOctree.serializer[D, Dim, PlacedNode]
      val quadH = dur.root { implicit tx =>
        DeterministicSkipOctree.empty[D, Dim, PlacedNode](IntSquare(extent, extent, extent))
      }

      new QuadGraphDB {
        val system: D = dur
        val handle: stm.Source[D#Tx, Tpe] = quadH
      }
    }
  }
  trait QuadGraphDB {
    implicit val system: D
    val handle: stm.Source[D#Tx, QuadGraphDB.Tpe]
  }

  def run(name: String): Unit = {
    val somDir = file("database") / s"${name}_som"
    if (somDir.isDirectory) {
      println(s"Directory $somDir already exists. Not regenerating.")
      val db = QuadGraphDB.open(name)
      import db.system
      Swing.onEDT(guiInit[D](db.handle))
      return
    }

    val stat = SOMMinMax.read(name)

    val placedNodesFut = Processor[Vec[PlacedNode]]("lattice") { self =>
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

//      val featSpecMin = Array(-1554.8430,-21.7765,-4.5185,-5.2909,-3.6100,-4.1124,-3.9966,-3.6367,-2.4137,-2.2400,-2.4790,-4.5608,-6.6133)
//      val featSpecMax = Array(176.9208,55.4446,20.6956,10.6913,8.8621,6.5888,5.7932,4.1103,4.9481,3.7131,4.0103,3.8659,3.4231)
//      val featTempMin = Array(0.3567,-3.2570,-2.8579,-2.1212,-1.9764,-1.8338,-2.3039,-1.8459,-1.6096,-1.5539,-1.3541,-1.4128,-1.4109)
//      val featTempMax = Array(3100155267289.2250,3049041188044.5825,2937674475702.0060,2888555993728.8633,2603175515013.0580,2561710809328.4565,2572049306867.7030,2272804304747.4520,2409931779268.9175,2502593081103.6147,2758321940498.5825,2663814737416.1343,2637628956055.0970)

      val (statSpectral, statTemporal) = stat

      def normalize(w: Weight): Unit = {
        w.spectral.zipWithIndex.foreach { case (v, i) =>
          val norm = v.linlin(statSpectral(i)._1, statSpectral(i)._2, 0, 1)
          w.spectral(i) = norm
        }
        w.temporal.zipWithIndex.foreach { case (v, i) =>
          val norm = v.linlin(statTemporal(i)._1, statTemporal(i)._2, 0, 1)
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
        // def norm(v: Array[Double], min: Array[Double], max: Array[Double]): Vec[Double] =
        //  (0 until v.length).map { i => v(i).linlin(min(i), max(i), 0, 1) }

        def sqrDifSumSqrt(a: Array[Double], b: Array[Double]): Double = {
          var i = 0
          var sum = 0.0
          while (i < a.length) {
            val d = a(i) - b(i)
            sum += d * d
            i += 1
          }
          math.sqrt(sum)
        }

        val w1sn = w1.spectral // norm(w1.spectral, featSpecMin, featSpecMax)
        val w2sn = w2.spectral // norm(w2.spectral, featSpecMin, featSpecMax)
        val spectDist = sqrDifSumSqrt(w1sn, w2sn) // (w1sn zip w2sn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt

        val w1tn = w1.temporal // norm(w1.temporal, featTempMin, featTempMax)
        val w2tn = w2.temporal // norm(w2.temporal, featTempMin, featTempMax)
        val tempDist = sqrDifSumSqrt(w1tn, w2tn) // (w1tn zip w2tn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt

        (spectDist + tempDist) / 2
      }

      def coordDist(n1: Coord, n2: Coord): Double =
        math.sqrt((n1.x - n2.x).squared + (n1.y - n2.y).squared) // Euclidean distance

      val trainingSet   : Array[Node]     = nodes.scramble().take(MAX_NODES).toArray
      println(s"Input ${nodes.length} nodes. Training with ${trainingSet.length} items.")
      val numIterations : Int             = trainingSet.length // * 4
      val mapRadius     : Double          = extent
      val timeConstant  : Double          = numIterations / math.log(mapRadius)

      def neighbourhoodRadius(iter: Double) = mapRadius * math.exp(-iter / timeConstant)

      def mkLattice(): Lattice = {
        val nodes = for {
          x <- 0 until (extent << 1) by gridStep
          y <- 0 until (extent << 1) by gridStep
        } yield {
            val c = Coord(x, y)
            val w = rndWeight()
            new PlacedWeight(c, w)
          }

        new Lattice(/* size, */ nodes.toArray)
      }

      val lattice = mkLattice()

      def bmu(iw: Weight): PlacedWeight = {
        val nodes = lattice.nodes
        var i = 0
        var bestDist = Double.PositiveInfinity
        var bestNode: PlacedWeight = null
        while (i < nodes.length) {
          val n = nodes(i)
          val dist = weightDist(iw, n.weight)
          if (dist < bestDist) {
            bestDist = dist
            bestNode = n
          }
          i += 1
        }
        require(bestNode != null)
        bestNode
      }

      def bmuNeighbours(radius: Double, bmu: PlacedWeight, lattice: Lattice): Iterator[Dist] =
        lattice.nodes.iterator.map(n => Dist(n, coordDist(n.coord, bmu.coord))).filter(_.radius <= radius)

      def learningRate(iter: Double): Double =
        0.072 * math.exp(-iter / numIterations) // decays over time

      def theta(d2bmu: Double, radius: Double): Double =
        math.exp(-d2bmu.squared / (2 * radius.squared)) // learning proportional to distance

      def adjust(input: Weight, weight: Weight, learningRate: Double, theta: Double): Unit = {
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

        performA(input.spectral, weight.spectral)
        performA(input.temporal, weight.temporal)
      }

      def nextLattice(iter: Int): Unit = {
        // if (iter % 100 == 0) println(s"---- iter $iter")
        val randomInput   = trainingSet(iter % trainingSet.length) // .choose()
        val bmuNode       = bmu(randomInput.weight)
        val radius        = neighbourhoodRadius(iter)
        val inNodeIter    = bmuNeighbours(radius, bmuNode, lattice)
        val lRate         = learningRate(iter)
        val inNodeB = Vector.newBuilder[Dist]
        inNodeIter.foreach(inNodeB += _)
        inNodeB.result().par.foreach { dist =>
          val tTheta  = theta(dist.radius, radius)
          adjust(randomInput.weight, dist.node.weight, lRate, tTheta)
        }

        self.progress = (iter + 1).toDouble / numIterations * 0.5
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

      val placeNum = new AtomicInteger(0)

      val futRes = Future[Vec[PlacedNode]] {
        // val nodes = latticeN.nodes.collect { case PlacedNode(c, n: Node) => PlacedNode(c, n) }
        val nodes = trainingSet.zipWithIndex.par.map { case (in, idx) =>
          val nearest = bmu(in.weight)
          // self.progress = ((idx + 1).toDouble / 1000) * 0.5 + 0.5
          // self.checkAborted()
          placeNum.incrementAndGet()
          new PlacedNode(nearest.coord, in)
        } .toIndexedSeq
        // new Lattice(/* size = latticeSize, */ nodes = nodes)
        nodes
      }

      var done = false
      var res: Vec[PlacedNode] = null
      while (!done) try {
        res = Await.result(futRes, Duration(10, TimeUnit.SECONDS))
        done = true
      } catch {
        case _: TimeoutException =>
          self.progress = (placeNum.get().toDouble / trainingSet.length) * 0.5 + 0.5
          self.checkAborted()
      }

      res
    }

    // futWeights.monitor(printResult = false)

    placedNodesFut.monitor(printResult = false)
    placedNodesFut.onFailure {
      case ex =>
        println("lattice generation failed:")
        ex.printStackTrace()
    }

    placedNodesFut.onSuccess { case nodes =>
      println(s"Number of nodes: ${nodes.length}")

      val db = QuadGraphDB.open(name)
      import db.system
      system.step { implicit tx =>
        val quad = db.handle()
        nodes.foreach(quad.add)
      }
      Swing.onEDT(guiInit[D](db.handle))
    }

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }
  }

  def guiInit[R <: Sys[R]](quadH: stm.Source[R#Tx, DeterministicSkipOctree[R, Dim, PlacedNode]])
                          (implicit cursor: stm.Cursor[R]): Unit = {

    var synthOpt = Option.empty[Synth]

    import de.sciss.synth.Ops._

    val quadView  = new SkipQuadtreeView[R, PlacedNode](quadH, cursor, _.coord)

    def stopSynth(): Unit = synthOpt.foreach { synth =>
      synthOpt = None
      if (synth.server.isRunning) synth.release(3.0) // free()
    }
    def playSynth(): Unit = {
      stopSynth()
      for {
        s    <- Try(Server.default).toOption
        node <- quadView.highlight.headOption
      } {
        val graph = node.node.input.graph // node.chromosome.graph
        val df    = SynthDef("test", graph.expand(DefaultUGenGraphBuilderFactory))
        val x     = df.play(s, args = Seq("out" -> 1))
        synthOpt = Some(x)
      }
    }

    val pStatus = new ServerStatusPanel
    def boot(): Unit = {
      val cfg = Server.Config()
      cfg.pickPort()
      val connect = Server.boot(config = cfg) {
        case ServerConnection.Running(s) =>
        case ServerConnection.Aborted    =>
      }
      pStatus.booting = Some(connect)
    }

    val butKill = Button("Kill") {
      import scala.sys.process._
      Try(Server.default).toOption.foreach(_.dispose())
      "killall scsynth".!
    }

    pStatus.bootAction = Some(boot)
    val bs = Transport.makeButtonStrip(Seq(Transport.Stop(stopSynth()), Transport.Play(playSynth())))
    val tp = new FlowPanel(pStatus, butKill, bs)

    // quadView.scale = 240.0 / extent
    val quadComp  = Component.wrap(quadView)
    new MainFrame {
      contents = new BorderPanel {
        add(tp      , BorderPanel.Position.North )
        add(quadComp, BorderPanel.Position.Center)
      }
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

        val nodeOpt = cursor.step { implicit tx =>
          val q = quadH()
          q.nearestNeighborOption(IntPoint2D(x, y), IntDistanceMeasure2D.euclideanSq)
        }
        nodeOpt.foreach { node =>
          // println(node.node.input.graph)
          // --- the hightlight doesn't seem to be working,
          // probably because SynthGraph is not equal to itself after de-serialization?
          quadView.highlight = Set(node)
          quadView.repaint()
          bs.button(Transport.Play).foreach(_.doClick(100))
        }
    }
  }
}
