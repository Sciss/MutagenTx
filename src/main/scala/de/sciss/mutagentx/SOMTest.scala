package de.sciss.mutagentx

import java.util.concurrent.TimeUnit

import de.sciss.dsp
import de.sciss.file._
import de.sciss.lucre.data.DeterministicSkipOctree
import de.sciss.lucre.data.gui.SkipQuadtreeView
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.geom.{IntPoint2D, IntSpace, IntSquare}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.InMemory
import de.sciss.processor.{Processor, ProcessorOps}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.{kollflitz, numbers}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}
import scala.swing.{Component, MainFrame, Swing}
import scala.util.Try

object SOMTest extends App {
  run()

  class Input (val graph: SynthGraph, val iter: Int, val fitness: Float)
  
  sealed trait HasWeight {
    type Self <: HasWeight
    def weight: Weight
    def replaceWeight(newWeight: Weight): Self
  }
  
  case class Node(graph: SynthGraph, iter: Int, fitness: Float, weight: Weight) extends HasWeight {
    override def toString = {
      val fitS  = f"$fitness%1.3f"
      s"Weight(graph size = ${graph.sources.size}, iter = $iter, fitness = $fitS, w = $weight)"
    }

    type Self = Node
    def replaceWeight(newWeight: Weight): Node = copy(weight = newWeight)
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

  type S = InMemory
  type D = IntSpace.TwoDim

  def run(): Unit = {
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
                val node    = Node(input.graph, iter = input.iter, fitness = input.fitness, weight = weight)
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
        0.072 * math.exp(-iter/numIterations) // decays over time

      def theta(d2bmu: Double, radius: Double): Double =
        math.exp(-d2bmu.squared/(2.0*radius.squared)) // learning proportional to distance

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
          val tTheta = theta(t._2, radius)
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
        implicit object nodeSerializer extends Serializer[S#Tx, S#Acc, PlacedNode[Node]] {
          def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): PlacedNode[Node] = sys.error("not supported")
          def write(v: PlacedNode[Node], out: DataOutput): Unit = sys.error("not supported")
        }
        implicit val pointView = (n: PlacedNode[Node], tx: S#Tx) => n.coord
        val quad = DeterministicSkipOctree.empty[S, D, PlacedNode[Node]](IntSquare(extent, extent, extent))
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

  def guiInit(quadH: stm.Source[S#Tx, DeterministicSkipOctree[S, D, PlacedNode[Node]]])(implicit system: S): Unit = {
    // val model = InteractiveSkipOctreePanel.makeModel2D(system)(())
    // new InteractiveSkipOctreePanel(model)
    val quadView = new SkipQuadtreeView[S, PlacedNode[Node]](quadH, system, _.coord)
    new MainFrame {
      contents = Component.wrap(quadView)
      pack().centerOnScreen()
      open()
    }
  }
}
