package de.sciss.mutagentx

import java.util.concurrent.TimeUnit

import de.sciss.dsp
import de.sciss.file._
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.Source
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{expr, stm}
import de.sciss.processor.{Processor, ProcessorOps}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Durable, SynthGraphs}
import de.sciss.synth.ugen.ConfigOut

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}
import scala.util.Try

object SOMGenerator extends App {
  type S = ConfluentReactive
  type D = Durable

  run(name      = if (args.length > 0) args(0) else "betanovuss0",
      audioName = if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")

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
      s"Input(graph size = ${graph.sources.size}, iter = $iter, fitness = $fitS)"
    }
  }

  import Algorithm.executionContext

  object SynthGraphDB {
    type Tpe = expr.List.Modifiable[D, expr.List.Modifiable[D, Node, Unit], Unit]

    def open(name: String): SynthGraphDB = {
      val dir = file("database") / s"${name}_def"
      val dur = Durable(BerkeleyDB.factory(dir))
      implicit val listSer = expr.List.Modifiable.serializer[D, Node](Node.serializer)
      implicit val iterSer = expr.List.Modifiable.serializer[D, expr.List.Modifiable[D, Node, Unit]]
      val iterListH = dur.root { implicit tx =>
        expr.List.Modifiable[D, expr.List.Modifiable[D, Node, Unit]]
      }
      new SynthGraphDB {
        val system: D = dur
        val handle: Source[D#Tx, Tpe] = iterListH
      }
    }
  }
  trait SynthGraphDB {
    implicit val system: D
    val handle: stm.Source[D#Tx, SynthGraphDB.Tpe]
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
  case class Node(input: Input, weight: Weight)

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
        new Weight(spectral = spectral, temporal = temporal)
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
  class Weight(val spectral: Array[Double], val temporal: Array[Double]) {
    override def toString = spectral.map(d => f"$d%1.3f").mkString("[", ", ", "]")
  }

  // hashes: from previous iterations, prevents that multiple identical synth graphs appear
  def analyzeIter(a: Algorithm[S], path: S#Acc, hashes: Set[Int]): Future[(Vec[Node], Set[Int])] = {
    ???
//    val csr = a.global.forkCursor
//    val futGraphs: Future[(Set[Int], Vec[Input])] = Future {
//      val res: (Set[Int], Vec[Input]) = csr.stepFrom(path) { implicit tx =>
//        val g     = a.genome
//        val cs    = g.chromosomes()
//        val fit   = g.fitness    ()
//        val iter  = tx.inputAccess.size / 2
//        val sel0  = (cs zip fit).filter(_._2 > 0.2)
//        println(s"No. of chromosomes fit enough: ${sel0.size}")
//        val sel   = sel0 // .take(50) // for now
//        val res0 = ((hashes, Vec.empty[Input]) /: sel) { case ((hashesIn, inputsIn), (c, f)) =>
//          val gr    = impl.ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = false, config = true)
//          val hash  = gr.hashCode()
//          if (hashesIn.contains(hash)) (hashesIn, inputsIn) else {
//            val input = new Input(gr, iter = iter, fitness = f)
//            (hashesIn + hash, inputsIn :+ input)
//          }
//        }
//        res0
//      }
//      res
//    }
//
//    val mCfgB = dsp.MFCC.Config()
//    mCfgB.fftSize   = 1024
//    mCfgB.minFreq   = 60
//    mCfgB.maxFreq   = 16000
//    mCfgB.sampleRate= 44100.0
//    mCfgB.numCoeff  = 13
//    val mCfg        = mCfgB.build
//    import mCfg.{fftSize, numCoeff}
//    val mfcc        = dsp.MFCC(mCfg)
//    val fftSizeH    = fftSize/2
//
//    def futWeightsFun(proc: Processor[Any] with Processor.Body): (Vec[Node], Set[Int]) = {
//      val (hashesOut, graphs) = Await.result(futGraphs, Duration.Inf)
//      val numGraphs = graphs.size
//      val nodes = graphs.zipWithIndex.flatMap { case (input, gIdx) =>
//        val f         = File.createTemp(suffix = ".aif")
//        val futBounce = impl.EvaluationImpl.bounce(input.graph, audioF = f, inputSpec = a.inputSpec)
//        val resBounce = Try(Await.result(futBounce, Duration(20, TimeUnit.SECONDS)))
//
//        if (resBounce.isFailure) {
//          println("Bounce failed:")
//          println(resBounce)
//          None
//        } else {
//          import Util._
//
//          blocking {
//            val af = AudioFile.openRead(f)
//            // if (gIdx == 29) {
//            //   println("AQUI")
//            // }
//            try {
//              val inBuf   = af.buffer(fftSize)
//              val winBuf  = new Array[Float](fftSize)
//              val win     = dsp.Window.Kaiser6.create(fftSize)
//              var off     = 0
//              val numFrames = af.numFrames
//              var remain  = numFrames
//              val mean    = new Array[Double](numCoeff)
//              val enBuf   = new Array[Double](fftSize)
//              var count   = 0
//              while (remain > 0) {
//                val chunk = math.min(remain, fftSize - off).toInt
//                af.read(inBuf, off, chunk)
//                val off1 = off + chunk
//                System.arraycopy(inBuf(0), 0, winBuf, 0, off1)
//                if (off1 < fftSize) java.util.Arrays.fill(winBuf, off1, fftSize, 0f)
//                mul(winBuf, 0, win, 0, off1)
//                if (count < fftSize) {
//                  val e = energy(winBuf, 0, off1)
//                  enBuf(count) = e
//                  // println(s"---- ENERGY: $e")
//                }
//                val coeff = mfcc.process(winBuf, 0, off1)
//                add(mean, 0, coeff, 0, numCoeff)
//                remain -= chunk
//                System.arraycopy(inBuf(0), fftSizeH, inBuf(0), 0, fftSizeH) // overlap
//                off = fftSizeH
//                count += 1
//
//                proc.progress = (((numFrames - remain).toDouble / numFrames) + gIdx) / numGraphs
//                proc.checkAborted()
//              }
//              if (count > 0) mul(mean, 0, numCoeff, 1.0 / count)
//
//              val temporal = Util.dct(enBuf, off = 0, len = count, numCoeff = numCoeff)
//              // println(s"temporal.sum = ${temporal.sum}")
//
//              if (mean.exists(x => x.isNaN || x.isInfinity) || enBuf.exists(_ > 1.0)) {
//                println("Dropping chromosome with NaN / exploding features!")
//                None
//              } else {
//                if (temporal(1) > 100) {
//                  println(s"Temporal exploded !? $gIdx")
//                }
//
//                val weight  = new Weight(spectral = mean, temporal = temporal)
//                val node    = Node(input, weight)
//                Some(node)
//              }
//            } finally {
//              af.cleanUp()
//              f.delete()
//            }
//          }
//        }
//      }
//      (nodes, hashesOut)
//    }
//    val futWeights = Processor[(Vec[Node], Set[Int])]("Weights")(futWeightsFun)
//
//      futWeights
  }

  def run(name: String, audioName: String): Unit = {
    ConfigOut.NO_NORMALIZE = true // yukk

    val dir   = file("database"  ) / name
    val in    = file("audio_work") / audioName // (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
    val store = dir.parent / s"${dir.name}_def"
    if (store.isDirectory) {
      println(s"Directory $store already exists. Not regenerating.")
      return
    }

    val a       = Algorithm(dir = dir, input = in)
    val csr     = a.global.cursor
    val path    = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
    val numIter = path.size / 2

    val graphDB = SynthGraphDB.open(name)
    import graphDB.{handle => iterListH, system => dur}

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

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }

    proc.monitor()
    proc.onFailure {
      case ex =>
        println("synth def database generation failed:")
        ex.printStackTrace()
    }

    proc.onComplete {
      case _ =>
        println("...complete")
        dur.close()
        sys.exit()
    }

    // Swing.onEDT {}  // keep JVM running
  }
}
