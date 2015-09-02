/*
 *  SOMGenerator2.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx

import java.util.concurrent.TimeUnit

import de.sciss.file._
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Source, Sys, TxnLike}
import de.sciss.lucre.{confluent, event => evt, expr, stm}
import de.sciss.mutagentx.SOMGenerator.Input
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.Durable
import de.sciss.synth.ugen.ConfigOut
import de.sciss.{dsp, numbers}

import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor
import scala.concurrent.{Await, Future, blocking}
import scala.util.Try

/** SOMGenerator in Grenzwerte Project. */
object SOMGenerator2 {
  type S = confluent.Confluent
  type D = Durable

  case class Config(dbName: String = "", audioName: String = "", iterStart: Int = 1, iterEnd: Int = 0,
                    iterStep: Int = 1, skipMissing: Boolean = false, append: Boolean = false)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("SOMGenerator2") {
      opt[String]('d', "database") required() text "database name"     action { (x, c) => c.copy(dbName    = x) }
      opt[String]('t', "target"  ) required() text "target audio name" action { (x, c) => c.copy(audioName = x) }
      opt[Int]("start")            text "start iteration"              action { (x, c) => c.copy(iterStart = x) }
      opt[Int]("end"  ) required() text "end iteration (inclusive)"    action { (x, c) => c.copy(iterEnd   = x) }
      opt[Int]("step" )            text "iteration step"               action { (x, c) => c.copy(iterStep  = x) }
      opt[Unit]('s', "skip-missing") text "skip missing files"         action { (_, c) => c.copy(skipMissing = true) }
      opt[Unit]('a', "append")     text "append to existing database"  action { (_, c) => c.copy(append = true) }
    }
    parser.parse(args, Config()).fold(sys.exit(1)) { cfg =>
      import cfg._
      require(iterStart <= iterEnd, s"Iteration start $iterStart must not be greater than end $iterEnd")
      require(iterStep > 0, s"Iteration step $iterStep must be greater than zero")

      run(dbName = dbName, audioName = audioName, iterStart = iterStart, iterEnd = iterEnd, iterStep = iterStep,
        skipMissing = skipMissing, append = append)
    }
  }

  import Algorithm.executionContext

  object SynthGraphDB {
    type ListAux[~ <: Sys[~]] = expr.List.Modifiable[~, Node[~]]
    type Tpe = expr.List.Modifiable[D, ListAux[D]]

    def mkDir(name: String): File = file("database") / s"${name}_def"

    def open(name: String): SynthGraphDB = {
      val dir = mkDir(name)
      val dur = Durable(BerkeleyDB.factory(dir))
      implicit val listSer = expr.List.Modifiable.serializer[D, Node[D]] // (Node.serializer)
      implicit val iterSer = expr.List.Modifiable.serializer[D, ListAux[D]]
      val iterListH = dur.root { implicit tx =>
        expr.List.Modifiable[D, ListAux]
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

  object Node extends Elem.Type {
    def typeID: Int = 0x4E6F6400

    def readIdentifiedObj[T <: Sys[T]](in: DataInput, access: T#Acc)(implicit tx: T#Tx): Node[T] = {
      val cookie  = in.readByte()
      require(cookie == 3)  // 'constant'
      val input   = Input .serializer.read(in)
      val weight  = Weight.serializer.read(in)
      Node(input = input, weight = weight)
    }

    implicit def serializer[T <: Sys[T]]: Serializer[T#Tx, T#Acc,Node[T]] = anySer.asInstanceOf[Ser[T]]

    private val anySer = new Ser[NoSys]

    private final class Ser[T <: Sys[T]] extends stm.impl.ElemSerializer[T, Node[T]] {
      def tpe = Node
    }
  }
  case class Node[T <: Sys[T]](input: Input, weight: Weight) extends stm.impl.ConstElemImpl[T] {
    def tpe: Elem.Type = Node

    def changed: EventLike[T, Any] = evt.Dummy[T, Any]

    def copy[Out <: Sys[Out]]()(implicit tx: T#Tx, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
      new Node[Out](input, weight)

    protected def writeData(out: DataOutput): Unit = {
      Input .serializer.write(input , out)
      Weight.serializer.write(weight, out)
    }
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
  def analyzeIter(dbName: String, inputFile: File, inputSpec: AudioFileSpec, iter: Int,
                  hashes: Set[Int], skipMissing: Boolean): Future[(Vec[Node[D]], Set[Int])] = {
    val futGraphs: Future[(Set[Int], Vec[Input])] = Future {
      val dir     = file("database") / dbName
      val f       = dir.parent / s"${dir.name}_iter$iter.bin"
      val inputs = if (skipMissing && !f.isFile) Vec.empty[Input] else IterPlayback.open(f)

      val res = ((hashes, Vec.empty[Input]) /: inputs) { case ((hashesIn, inputsIn), in) =>
        val graph = in.graph
        val ug    = graph.expand(DefaultUGenGraphBuilderFactory)
        val hash  = ug.hashCode()
        if (hashesIn.contains(hash)) (hashesIn, inputsIn) else {
          val input = new Input(graph, iter = in.iter, fitness = in.fitness)
          (hashesIn + hash, inputsIn :+ input)
        }
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

    def futWeightsFun(proc: Processor[Any] with Processor.Body): (Vec[Node[D]], Set[Int]) = {
      val (hashesOut, graphs) = Await.result(futGraphs, Duration.Inf)
      val numGraphs = graphs.size
      val nodes = graphs.zipWithIndex.flatMap { case (input, gIdx) =>
        val bounceF   = File.createTemp(suffix = ".aif")
        val futBounce = impl.EvaluationImpl.bounce(input.graph, audioF = bounceF, inputSpec = inputSpec)
        val resBounce = Try(Await.result(futBounce, Duration(20, TimeUnit.SECONDS)))

        if (resBounce.isFailure) {
          println("Bounce failed:")
          println(resBounce)
          None
        } else {
          import Util._

          blocking {
            val af = AudioFile.openRead(bounceF)
            // if (gIdx == 29) {
            //   println("AQUI")
            // }
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
                  // println(s"---- ENERGY: $e")
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
              // println(s"temporal.sum = ${temporal.sum}")

              if (mean.exists(x => x.isNaN || x.isInfinity) || enBuf.exists(_ > 1.0)) {
                println("Dropping chromosome with NaN / exploding features!")
                None
              } else {
                if (temporal(1) > 100) {
                  println(s"Temporal exploded !? $gIdx")
                }

                val weight  = new Weight(spectral = mean, temporal = temporal)
                val node    = Node[D](input, weight)
                Some(node)
              }
            } finally {
              af.cleanUp()
              bounceF.delete()
            }
          }
        }
      }
      (nodes, hashesOut)
    }
    val futWeights = Processor[(Vec[Node[D]], Set[Int])]("Weights")(futWeightsFun)

    futWeights
  }

  def run(dbName: String, audioName: String, iterStart: Int, iterEnd: Int, iterStep: Int, skipMissing: Boolean,
          append: Boolean): Unit = {
    // ConfigOut.NORMALIZE = true
    ConfigOut.LIMITER   = true
    ConfigOut.AMP       = true

    val store = SynthGraphDB.mkDir(dbName)
    if (!append && store.isDirectory) {
      println(s"Directory $store already exists. Not regenerating.")
      return
    }

    val graphDB = SynthGraphDB.open(dbName)
    import graphDB.{handle => iterListH, system => dur}

    val audioInput  = file("audio_work") / audioName

    val proc = Processor[Unit]("gen-def") { self =>
      val specFut     = TxnExecutor.defaultAtomic { implicit itx =>
        implicit val tx = TxnLike.wrap(itx)
        impl.EvaluationImpl.getInputSpec(audioInput)
      }
      val (inputFile, inputSpec) = Await.result(specFut, Duration.Inf)

      var hashes = Set.empty[Int]
      for (iter <- iterStart to iterEnd by iterStep) {
        import numbers.Implicits._
        val targetProg = iter.linlin(iterStart, iterEnd, 0, 1)
        println(f"-------- GENERATING SYNTH DEFS FOR ITERATION $iter (-> ${targetProg * 100}%1.1f%%) --------")
        val fut = analyzeIter(dbName = dbName, inputFile = inputFile, inputSpec = inputSpec,
          iter = iter, hashes = hashes, skipMissing = skipMissing)
        val (inputs: Vec[Node[D]], hashesOut: Set[Int]) = Await.result(fut, Duration.Inf)
        hashes = hashesOut
        println(s"...produced ${inputs.size} graphs (hash set now has size ${hashes.size}")
        dur.step { implicit tx =>
          val iterList = iterListH()
          val defList  = expr.List.Modifiable[D, Node]
          inputs.foreach(defList.addLast)
          iterList.addLast(defList)
        }
        self.progress = targetProg
        self.checkAborted()
      }
    }

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }

    // proc.monitor()
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
