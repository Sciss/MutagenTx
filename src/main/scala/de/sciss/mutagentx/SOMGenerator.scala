package de.sciss.mutagentx

import java.util.concurrent.TimeUnit

import de.sciss.dsp
import de.sciss.file._
import de.sciss.lucre.data.DeterministicSkipOctree
import de.sciss.lucre.data.gui.SkipQuadtreeView
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.geom.{IntPoint2D, IntSpace, IntSquare}
import de.sciss.lucre.{expr, stm}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.InMemory
import de.sciss.processor.{Processor, ProcessorOps}
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput, Serializer}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Durable, SynthGraphs}
import de.sciss.{kollflitz, numbers}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, blocking}
import scala.swing.{Component, MainFrame, Swing}
import scala.util.Try

object SOMGenerator extends App {
  run()

  object Input {
    implicit object serializer extends ImmutableSerializer[Input] {
      private final val COOKIE = 0x496E7000 // "Inp\0"

      def read(in: DataInput): Input = {
        val cookie  = in.readInt()
        if (cookie != COOKIE) throw new IllegalStateException(s"Expected cookie ${COOKIE.toHexString} but found ${cookie.toHexString}")
        val graph   = SynthGraphs.ValueSerializer.read(in)
        val iter    = in.readShort()
        val fitness = in.readFloat()
        Input(graph, iter = iter, fitness = fitness)
      }

      def write(input: Input, out: DataOutput): Unit = {
        out.writeInt(COOKIE)
        SynthGraphs.ValueSerializer.write(input.graph, out)
        out.writeShort(input.iter)
        out.writeFloat(input.fitness)
      }
    }
  }
  case class Input(graph: SynthGraph, iter: Int, fitness: Float) {
    override def toString = {
      val fitS  = f"$fitness%1.3f"
      s"Weight(graph size = ${graph.sources.size}, iter = $iter, fitness = $fitS)"
    }
  }

  sealed trait HasWeight {
    type Self <: HasWeight
    def weight: Weight
    def replaceWeight(newWeight: Weight): Self
  }

  object Node {
    implicit object serializer extends ImmutableSerializer[Node] {
      private final val COOKIE = 0x4E6F6400 // "Nod\0"

      def read(in: DataInput): Node = {
        val cookie  = in.readInt()
        if (cookie != COOKIE) throw new IllegalStateException(s"Expected cookie ${COOKIE.toHexString} but found ${cookie.toHexString}")
        val input   = Input.serializer.read(in)
        val weight  = Weight.serializer.read(in)
        Node(input = input, weight = weight)
      }

      def write(node: Node, out: DataOutput): Unit = {
        out.writeInt(COOKIE)
        Input.serializer.write(node.input, out)
        Weight.serializer.write(node.weight, out)
      }
    }
  }
  case class Node(input: Input, weight: Weight) extends HasWeight {
    type Self = Node
    def replaceWeight(newWeight: Weight): Node = copy(weight = newWeight)
  }

  object Weight {
    implicit object serializer extends ImmutableSerializer[Weight] {
      private final val COOKIE = 0x57656900 // "Wei\0"

      private def readArray(in: DataInput): Array[Double] = {
        val sz = in.readShort()
        val a  = new Array[Double](sz)
        var i = 0
        while (i < sz) {
          a(i) = in.readDouble()
          i += 1
        }
        a
      }

      def read(in: DataInput): Weight = {
        val cookie = in.readInt()
        if (cookie != COOKIE) throw new IllegalStateException(s"Expected cookie ${COOKIE.toHexString} but found ${cookie.toHexString}")
        val spectral = readArray(in)
        val temporal = readArray(in)
        Weight(spectral = spectral, temporal = temporal)
      }

      private def writeArray(a: Array[Double], out: DataOutput): Unit = {
        out.writeShort(a.length)
        var i = 0
        while (i < a.length) {
          out.writeDouble(a(i))
          i += 1
        }
      }

      def write(w: Weight, out: DataOutput): Unit = {
        out.writeInt(COOKIE)
        writeArray(w.spectral, out)
        writeArray(w.temporal, out)
      }
    }
  }
  case class Weight(spectral: Array[Double], temporal: Array[Double]) extends HasWeight {
    override def toString = spectral.map(d => f"$d%1.3f").mkString("[", ", ", "]")
    
    def weight = this
    type Self = Weight
    def replaceWeight(newWeight: Weight): Weight = newWeight
  }

  type AnyNode = PlacedNode[HasWeight]

  // case class Coord(x: Int, y: Int)
  type Coord = IntPoint2D
  val  Coord = IntPoint2D

  case class PlacedNode[+N](coord: Coord, node: N)

  case class Lattice[+N <: HasWeight](size: Int, nodes: Vec[PlacedNode[N]])

  type AnyLattice = Lattice[HasWeight]

  type I    = InMemory
  type Dim  = IntSpace.TwoDim

  import Algorithm.executionContext

  // hashes: from previous iterations, prevents that multiple identical synth graphs appear
  def analyzeIter(a: Algorithm, path: S#Acc, hashes: Set[Int]): Future[(Vec[Node], Set[Int])] = {
    val csr = a.global.forkCursor
    val futGraphs: Future[(Set[Int], Vec[Input])] = Future {
      val res: (Set[Int], Vec[Input]) = csr.stepFrom(path) { implicit tx =>
        val g     = a.genome
        val cs    = g.chromosomes()
        val fit   = g.fitness    ()
        val iter  = tx.inputAccess.size / 2
        val sel0  = (cs zip fit).filter(_._2 > 0.2)
        println(s"No. of chromosomes fit enough: ${sel0.size}")
        val sel   = sel0 // .take(50) // for now
        val res0 = ((hashes, Vec.empty[Input]) /: sel) { case ((hashesIn, inputsIn), (c, f)) =>
          val gr    = impl.ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = false, config = true)
          val hash  = gr.hashCode()
          if (hashesIn.contains(hash)) (hashesIn, inputsIn) else {
            val input = new Input(gr, iter = iter, fitness = f)
            (hashesIn + hash, inputsIn :+ input)
          }
        }
        res0
      }
      res
    }

    val mCfgB = dsp.MFCC.Config()
    mCfgB.fftSize   = 1024
    mCfgB.minFreq   = 60
    mCfgB.maxFreq   = 16000
    mCfgB.sampleRate= 44100.0
    mCfgB.numCoeff  = 13
    val mCfg        = mCfgB.build
    import mCfg.{fftSize, numCoeff}
    val mfcc        = dsp.MFCC(mCfg)
    val fftSizeH    = fftSize/2

    val futWeights = Processor[(Vec[Node], Set[Int])]("Weights") { proc =>
      val (hashesOut, graphs) = Await.result(futGraphs, Duration.Inf)
      val numGraphs = graphs.size
      val nodes = graphs.zipWithIndex.flatMap { case (input, gIdx) =>
        val f         = File.createTemp(suffix = ".aif")
        val futBounce = impl.EvaluationImpl.bounce(input.graph, audioF = f, inputSpec = a.inputSpec)
        val resBounce = Try(Await.result(futBounce, Duration(20, TimeUnit.SECONDS)))

        if (resBounce.isFailure) {
          println("Bounce failed:")
          println(resBounce)
          None
        } else {
          import Util._

          blocking {
            val af = AudioFile.openRead(f)
            try {
              val inBuf   = af.buffer(fftSize)
              val winBuf  = new Array[Float](fftSize)
              val win     = dsp.Window.Kaiser6.create(fftSize)
              var off     = 0
              val numFrames = af.numFrames
              var remain  = numFrames
              val mean    = new Array[Double](numCoeff)
              val enBuf   = new Array[Double](fftSize)
              var count   = 0
              while (remain > 0) {
                val chunk = math.min(remain, fftSize - off).toInt
                af.read(inBuf, off, chunk)
                val off1 = off + chunk
                System.arraycopy(inBuf(0), 0, winBuf, 0, off1)
                if (off1 < fftSize) java.util.Arrays.fill(winBuf, off1, fftSize, 0f)
                mul(winBuf, 0, win, 0, off1)
                if (count < fftSize) {
                  val e = energy(winBuf, 0, off1)
                  enBuf(count) = e
                }
                val coeff = mfcc.process(winBuf, 0, off1)
                add(mean, 0, coeff, 0, numCoeff)
                remain -= chunk
                System.arraycopy(inBuf(0), fftSizeH, inBuf(0), 0, fftSizeH) // overlap
                off = fftSizeH
                count += 1

                proc.progress = (((numFrames - remain).toDouble / numFrames) + gIdx) / numGraphs
                proc.checkAborted()
              }
              if (count > 0) mul(mean, 0, numCoeff, 1.0 / count)

              val temporal = Util.dct(enBuf, off = 0, len = count, numCoeff = numCoeff)

              if (mean.exists(x => x.isNaN || x.isInfinity)) {
                println("Dropping chromosome with NaN features!")
                None
              } else {
                val weight  = Weight(spectral = mean, temporal = temporal)
                val node    = Node(input, weight)
                Some(node)
              }
            } finally {
              af.cleanUp()
              f.delete()
            }
          }
        }
      }
      (nodes, hashesOut)
    }

    futWeights
  }

  def run(): Unit = {
    val dir   = file("database"  ) / (if (args.length > 0) args(0) else "betanovuss0")
    val in    = file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
    val store = dir.parent / s"${dir.name}_def"
    if (store.isDirectory) {
      println(s"Directory $store already exists. Not regenerating.")
      return
    }

    val a       = Algorithm(dir = dir, input = in)
    val csr     = a.global.cursor
    val path    = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
    val numIter = path.size / 2

    val dur     = Durable(BerkeleyDB.factory(store))
    implicit val listSer = expr.List.Modifiable.serializer[D, Node]
    implicit val iterSer = expr.List.Modifiable.serializer[D, expr.List.Modifiable[D, Node, Unit]]
    val iterListH   = dur.root { implicit tx =>
      expr.List.Modifiable[D, expr.List.Modifiable[D, Node, Unit]]
    }

    val proc = Processor[Unit]("gen-def") { self =>
      var hashes = Set.empty[Int]
      for (i <- 1 to numIter) {
        println(s"-------- GENERATING SYNTH DEFS FOR ITERATION $i of $numIter --------")
        val fut = analyzeIter(a, path.take(i * 2), hashes)
        val (inputs: Vec[Node], hashesOut: Set[Int]) = Await.result(fut, Duration.Inf)
        hashes = hashesOut
        dur.step { implicit tx =>
          val iterList = iterListH()
          val defList  = expr.List.Modifiable[D, Node]
          inputs.foreach(defList.addLast)
          iterList.addLast(defList)
        }
        self.progress = i.toDouble / numIter
        self.checkAborted()
      }
    }

    proc.monitor()
    proc.onComplete {
      case _ =>
        dur.close()
        sys.exit()
    }

    Swing.onEDT {}  // keep JVM running
  }

  def runFOO(): Unit = {
    val dir = file("database"  ) / (if (args.length > 0) args(0) else "betanovuss0")
    val in  = file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
    val a   = Algorithm(dir = dir, input = in)
    val csr = a.global.cursor

    // (graph, fitness, iter)
    val graphs: Vec[Input] = csr.step { implicit tx =>
      val g     = a.genome
      val cs    = g.chromosomes()
      val fit   = g.fitness    ()
      val iter  = tx.inputAccess.size / 2
      val sel0  = (cs zip fit).filter(_._2 > 0.2)
      println(s"No. of chromosomes fit enough: ${sel0.size}")
      val sel   = sel0.take(50) // for now
      sel.map { case (c, f) =>
        val gr = impl.ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = false, config = true)
        new Input(gr, iter = iter, fitness = f)
      }
    }

    import Algorithm.executionContext

    val mCfgB = dsp.MFCC.Config()
    mCfgB.fftSize   = 1024
    mCfgB.minFreq   = 60
    mCfgB.maxFreq   = 16000
    mCfgB.sampleRate= 44100.0
    mCfgB.numCoeff  = 13
    val mCfg        = mCfgB.build
    import mCfg.{fftSize, numCoeff}
    val mfcc        = dsp.MFCC(mCfg)
    val fftSizeH    = fftSize/2
    
    val futWeights = Processor[Vec[Node]]("Weights") { proc =>
      val numGraphs = graphs.size
      graphs.zipWithIndex.flatMap { case (input, gIdx) =>
        val f         = File.createTemp(suffix = ".aif")
        val futBounce = impl.EvaluationImpl.bounce(input.graph, audioF = f, inputSpec = a.inputSpec)
        val resBounce = Try(Await.result(futBounce, Duration(20, TimeUnit.SECONDS)))

        if (resBounce.isFailure) {
          println("Bounce failed:")
          println(resBounce)
          None
        } else {
          import Util._

          blocking {
            val af = AudioFile.openRead(f)
            try {
              val inBuf   = af.buffer(fftSize)
              val winBuf  = new Array[Float](fftSize)
              val win     = dsp.Window.Kaiser6.create(fftSize)
              var off     = 0
              val numFrames = af.numFrames
              var remain  = numFrames
              val mean    = new Array[Double](numCoeff)
              val enBuf   = new Array[Double](fftSize)
              var count   = 0
              while (remain > 0) {
                val chunk = math.min(remain, fftSize - off).toInt
                af.read(inBuf, off, chunk)
                val off1 = off + chunk
                System.arraycopy(inBuf(0), 0, winBuf, 0, off1)
                if (off1 < fftSize) java.util.Arrays.fill(winBuf, off1, fftSize, 0f)
                mul(winBuf, 0, win, 0, off1)
                if (count < fftSize) {
                  val e = energy(winBuf, 0, off1)
                  enBuf(count) = e
                }
                val coeff = mfcc.process(winBuf, 0, off1)
                add(mean, 0, coeff, 0, numCoeff)
                remain -= chunk
                System.arraycopy(inBuf(0), fftSizeH, inBuf(0), 0, fftSizeH) // overlap
                off = fftSizeH
                count += 1

                proc.progress = (((numFrames - remain).toDouble / numFrames) + gIdx) / numGraphs
                proc.checkAborted()
              }
              if (count > 0) mul(mean, 0, numCoeff, 1.0 / count)

              val temporal = Util.dct(enBuf, off = 0, len = count, numCoeff = numCoeff)

              if (mean.exists(x => x.isNaN || x.isInfinity)) {
                println("Dropping chromosome with NaN features!")
                None
              } else {
                val weight  = Weight(mean, temporal)
                val node    = Node(input, weight)
                Some(node)
              }
            } finally {
              af.cleanUp()
              f.delete()
            }
          }
        }
      }
    }

    val latticeFut = futWeights.map[Lattice[Node]] { nodes =>

      // cf. http://oogifu.blogspot.com/2014/07/self-organizing-map-in-scala.html

      implicit val rnd = new util.Random

      import kollflitz.RandomOps._
      import numbers.Implicits._

      val featMin = Array(-1473.435, -2.223, -3.793, -4.728, -1.674, -1.988, -2.095, -2.003, -1.324, -1.997, -1.884, -3.046, -4.375)
      val featMax = Array(  151.118, 57.481, 20.539, 10.134, 8.485,   6.312,  5.287,  3.876,  4.552,  2.771,  3.031,  3.047,  3.272)

      def normalize(w: Weight): Weight = {
        val fNew = w.spectral.zipWithIndex.map { case (v, i) => v.linlin(featMin(i), featMax(i), 0, 1) }
        w.copy(spectral = fNew)
      }
      
      val nodesN = nodes.map(n => n.copy(weight = normalize(n.weight)))

      def rndWeight: Weight = {
        val spectral = Array.fill(numCoeff)(rnd.nextDouble())
        val temporal = Array.fill(numCoeff)(rnd.nextDouble())
        Weight(spectral, temporal)
      }

      def weightDist(w1: Weight, w2: Weight): Double = {
        def norm(v: Array[Double]): Vec[Double] = (0 until v.length).map { i => v(i).linlin(featMin(i), featMax(i), 0, 1) }
        val w1n = norm(w1.spectral)
        val w2n = norm(w2.spectral)
        (w1n zip w2n).map { tup => (tup._1 - tup._2).squared } .sum.sqrt
      }

      def nodeDist(n1: AnyNode, n2: AnyNode): Double =
        math.sqrt((n1.coord.x - n2.coord.x).squared + (n1.coord.y-n2.coord.y).squared) // Euclidean distance

      val latticeSize   : Int             = 100   // XXX
      val trainingSet   : Vec[HasWeight]  = nodesN
      val numIterations : Int             = trainingSet.size * 4 // XXX
      val mapRadius     : Double          = latticeSize / 2.0
      val timeConstant  : Double          = numIterations / math.log(mapRadius)

      def neighbourhoodRadius(iter: Double) = mapRadius * math.exp(-iter/timeConstant)

      def bmu(input: HasWeight, lattice: Lattice[HasWeight]): PlacedNode[HasWeight] = {
        val sortedNodesByDist = lattice.nodes.sortBy(n => weightDist(input.weight, n.node.weight))
        sortedNodesByDist(0)
      }

      def bmuNeighbours(radius: Double, bmu: AnyNode,
                        lattice: AnyLattice): (Vec[(AnyNode, Double)], Vec[(AnyNode, Double)]) =
        lattice.nodes.map(n => (n, nodeDist(n, bmu))).partition(n => n._2 <= radius)

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
        val randomInput   = trainingSet.choose()
        val bmuNode       = bmu(randomInput, lattice)
        val radius        = neighbourhoodRadius(iter)
        val allNodes      = bmuNeighbours(radius, bmuNode, lattice)
        val lRate         = learningRate(iter)
        val adjustedNodes = allNodes._1.par.map { t =>
          val tTheta  = theta(t._2, radius)
          val nWeight = adjust(randomInput, t._1.node.weight, lRate, tTheta)
          PlacedNode(t._1.coord, nWeight)
        } .toIndexedSeq

        new Lattice(lattice.size, adjustedNodes ++ allNodes._2.map(_._1))
      }

      def mkLattice(size: Int): AnyLattice = {
        val nodes = for {
          x <- 0 until size
          y <- 0 until size
        } yield PlacedNode[HasWeight](Coord(x, y), rndWeight)

        new Lattice(size, nodes)
      }

      @tailrec def run(in: AnyLattice, iter: Int): AnyLattice =
        if (iter >= numIterations) in else {
          val out = nextLattice(iter = iter, lattice = in)
          run(out, iter + 1)
        }

      val lattice0 = mkLattice(size = latticeSize)
      val latticeN = run(lattice0, iter = 0)
      val res: Lattice[Node] = {
        val nodes = latticeN.nodes.collect { case PlacedNode(c, n: Node) => PlacedNode(c, n) }
        Lattice(size = latticeSize, nodes = nodes)
      }

      res
    }

    futWeights.monitor(printResult = false)
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

      import numbers.Implicits._
      val extent  = ((lattice.size + 1) / 2).nextPowerOfTwo
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

    Swing.onEDT {}  // keep JVM running :)
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
    new MainFrame {
      contents = Component.wrap(quadView)
      pack().centerOnScreen()
      open()
    }
  }
}
