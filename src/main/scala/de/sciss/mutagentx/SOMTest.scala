package de.sciss.mutagentx

import java.util
import java.util.concurrent.TimeUnit

import de.sciss.dsp.{Window, MFCC}
import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.processor.{Processor, ProcessorOps}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile

import scala.concurrent.{Await, blocking}
import scala.concurrent.duration.Duration
import scala.swing.Swing
import scala.util.Try

object SOMTest extends App {
  run()

  class Input (val graph: SynthGraph, val iter: Int, val fitness: Float)
  class Weight(val graph: SynthGraph, val iter: Int, val fitness: Float, val feature: Array[Double]) {
    override def toString = {
      val fitS  = f"$fitness%1.3f"
      val featS = feature.map(d => f"$d%1.3f").mkString("[", ", ", "]")
      s"Weight(graph size = ${graph.sources.size}, iter = $iter, fitness = $fitS, features = $featS)"
    }
  }

  /** Mutates `a` by multiplying its contents with `b`. */
  def mul(a: Array[Float], aOff: Int, b: Array[Float], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) *= b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by adding `b` to it. */
  def add(a: Array[Double], aOff: Int, b: Array[Double], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) += b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by multiplying each element with `f` */
  def mul(a: Array[Double], off: Int, len: Int, f: Double): Unit = {
    var ai = off
    val stop = ai + len
    while (ai < stop) {
      a(ai) *= f
      ai += 1
    }
  }

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
      val sel   = sel0 // .take(10) // for now
      sel.map { case (c, f) =>
        val gr = impl.ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = true)
        new Input(gr, iter = iter, fitness = f)
      }
    }

    import Algorithm.executionContext

    val futWeights = Processor[Vec[Weight]]("Weights") { proc =>
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
          val mCfgB = MFCC.Config()
          mCfgB.fftSize   = 1024
          mCfgB.minFreq   = 60
          mCfgB.maxFreq   = 16000
          mCfgB.sampleRate= 44100.0
          mCfgB.numCoeff  = 13
          val mCfg        = mCfgB.build
          import mCfg.{fftSize, numCoeff}
          val mfcc        = MFCC(mCfg)
          val fftSizeH    = fftSize/2

          blocking {
            val af = AudioFile.openRead(f)
            try {
              val inBuf   = af.buffer(fftSize)
              val winBuf  = new Array[Float](fftSize)
              val win     = Window.Kaiser6.create(fftSize)
              var off     = 0
              var remain  = af.numFrames
              val mean    = new Array[Double](numCoeff)
              var count   = 0
              while (remain > 0) {
                val chunk = math.min(remain, fftSize - off).toInt
                af.read(inBuf, off, chunk)
                val off1 = off + chunk
                System.arraycopy(inBuf(0), 0, winBuf, 0, off1)
                if (off1 < fftSize) util.Arrays.fill(winBuf, off1, fftSize, 0f)
                mul(winBuf, 0, win, 0, off1)
                val coeff = mfcc.process(winBuf, 0, off1)
                add(mean, 0, coeff, 0, numCoeff)
                remain -= chunk
                System.arraycopy(inBuf(0), fftSizeH, inBuf(0), 0, fftSizeH) // overlap
                off = fftSizeH
                count += 1

                proc.progress = (((af.numFrames - remain).toDouble / remain) + gIdx) / numGraphs
                proc.checkAborted()
              }
              if (count > 0) mul(mean, 0, numCoeff, 1.0 / count)
              if (mean.exists(x => x.isNaN || x.isInfinity)) {
                println("Dropping chromosome with NaN features!")
                None
              } else {
                val w = new Weight(input.graph, iter = input.iter, fitness = input.fitness, feature = mean)
                Some(w)
              }
            } finally {
              af.cleanUp()
              f.delete()
            }
          }
        }
      }
    }

    futWeights.monitor(printResult = false)
    futWeights.onSuccess {
      case ws =>
        println(s"Number of weights: ${ws.size}")
        import kollflitz.Ops._
        val perc = (0 until 13).map { i =>
          val all = ws.map { w =>
            w.feature(i)
          }
          val sorted = all.sortedT
          (sorted.percentile(2), sorted.percentile(98))
        }
        println("Feature vector  2% percentiles:")
        println(perc.map(tup => f"${tup._1}%1.3f").mkString("[", ",", "]"))
        println("Feature vector 98% percentiles:")
        println(perc.map(tup => f"${tup._2}%1.3f").mkString("[", ",", "]"))
        // ws.foreach(println)
    }

    Swing.onEDT {
      guiInit()
    }
  }

  /*

Feature vector  2% percentiles:
[-1473.435,-2.223,-3.793,-4.728,-1.674,-1.988,-2.095,-2.003,-1.324,-1.997,-1.884,-3.046,-4.375]
Feature vector 98% percentiles:
[151.118,57.481,20.539,10.134,8.485,6.312,5.287,3.876,4.552,2.771,3.031,3.047,3.272]


   */

  def guiInit(): Unit = {
    // println("Aqui")
  }
}
