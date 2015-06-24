/*
 *  EvaluationImpl.scala
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
package impl

import java.util.concurrent.TimeUnit

import de.sciss.file._
import de.sciss.filecache.{TxnConsumer, TxnProducer}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.InMemory
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.Span
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction, Strugatzki}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.{Bounce, ExprImplicits, Obj, Proc, Timeline, WorkspaceHandle}
import de.sciss.{filecache, numbers}

import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor
import scala.concurrent.{Await, Future, Promise, TimeoutException, blocking}

object EvaluationImpl {
  val DEBUG = false

  private object CacheValue {
    implicit object serializer extends ImmutableSerializer[CacheValue] {
      def write(v: CacheValue, out: DataOutput): Unit = {
        out.writeLong(v.lastModified)
        out.writeUTF (v.meta   .getCanonicalPath)
        out.writeUTF (v.feature.getCanonicalPath)
      }

      def read(in: DataInput): CacheValue = {
        val mod     = in.readLong()
        val meta    = file(in.readUTF())
        val feature = file(in.readUTF())
        CacheValue(lastModified = mod, meta = meta, feature = feature)
      }
    }
  }

  private type CacheKey = (File, Int)
  private case class CacheValue(lastModified: Long, meta: File, feature: File)

  private val cCfg  = {
    val c = filecache.Config[CacheKey, CacheValue]()
    c.executionContext = Algorithm.executionContext
    c.capacity  = filecache.Limit(count = 10)
    c.accept    = { (key, value) => key._1.lastModified() == value.lastModified }
    c.space     = { (key, value) => value.meta.length() + value.feature.length() }
    c.evict     = { (key, value) => value.meta.delete() ; value.feature.delete() }
    c.build
  }
  private val cacheP = TxnExecutor.defaultAtomic { implicit tx => TxnProducer(cCfg) }
  private val cache  = TxnConsumer(cacheP)(mkCacheValue)

  import cacheP.executionContext

  private def mkCacheValue(key: (File, Int)): Future[CacheValue] = {
    val (f, numCoeffs) = key
    val inputSpec         = AudioFile.readSpec(f)
    val inputMod          = f.lastModified()
    require(inputSpec.numChannels == 1, s"Input file '${f.name}' must be mono but has ${inputSpec.numChannels} channels")
    val exCfg             = FeatureExtraction.Config()
    exCfg.audioInput      = f
    val inputFeature      = File.createTemp(suffix = ".aif")
    exCfg.featureOutput   = inputFeature
    val inputExtr         = File.createTemp(suffix = "_feat.xml")
    exCfg.metaOutput      = Some(inputExtr)
    exCfg.numCoeffs       = numCoeffs
    val futInputExtr      = FeatureExtraction(exCfg)
    futInputExtr.start()
    futInputExtr.map { _ =>
      CacheValue(lastModified = inputMod, meta = inputExtr, feature = inputFeature)
    }
  }

  def getInputSpec(input: File)(implicit tx: TxnLike): Future[(File, AudioFileSpec)] = {
    val key       = input -> Algorithm.numCoeffs
    val futMeta   = cache.acquire(key)(tx.peer)
    val res       = futMeta.map { v =>
      val inputExtr = v.meta
      val inputSpec = blocking(AudioFile.readSpec(input))
      (inputExtr, inputSpec)
    }
    res.onComplete { case _ => TxnExecutor.defaultAtomic { implicit tx => cache.release(key) }}
    res
  }

  //  def apply(c: Chromosome, algorithm: Algorithm)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[Evaluated] = {
  //    val futEval = getInputSpec(algorithm.input).flatMap { case (inputExtr, inputSpec) =>
  //      if (Txn.findCurrent.isEmpty) {
  //        cursor.step { implicit tx =>
  //          evaluate(c, algorithm, inputSpec, inputExtr)
  //        }
  //      } else {
  //        evaluate(c, algorithm, inputSpec, inputExtr)
  //      }
  //    }
  //    // val fitness = Await.result(futEval, Duration(24, TimeUnit.SECONDS) /* Duration.Inf */).fitness
  //    // fitness
  //    futEval
  //  }

  private val featNorms = Array[Array[Float]](
    Array(0.006015186f,1.4569731f),
    Array(-1.4816481f,3.093808f),
    Array(-1.4089416f,1.267046f),
    Array(-0.860692f,1.4034394f),
    Array(-0.65952975f,1.431201f),
    Array(-0.66072506f,0.8506244f),
    Array(-0.2808966f,0.90672106f),
    Array(-0.29912513f,0.705802f),
    Array(-0.22443223f,0.67802113f),
    Array(-0.1471797f,0.68207365f),
    Array(-0.104354106f,0.6723507f),
    Array(-0.2412649f,0.70821077f),
    Array(-0.16983563f,0.6771785f),
    Array(-0.10048226f,0.64655834f)
  )

  private val inMemory = InMemory()

  /** Bounces a synth def to an audio file.
    *
    * @param graph       the synth graph to play and evaluate
    * @param audioF      the audio output file to bounce to
    * @param inputSpec   the spec of the original target sound
    * @param duration0   the duration to bounce in seconds or `-1` to bounce the duration of the target sound
    */
  def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, duration0: Double = -1): Processor[Any] = {
    type I  = InMemory
    implicit val iCursor = inMemory

    val exp = ExprImplicits[I]
    import exp._

    val objH = inMemory.step { implicit tx =>
      val proc      = Proc[I]
      proc.graph()  = graph
      val procObj   = Obj(Proc.Elem(proc))
      tx.newHandle(procObj) // (Obj.typedSerializer[I, Proc.Elem[I]])
    }

    import WorkspaceHandle.Implicits._
    val bncCfg              = Bounce.Config[I]
    bncCfg.group            = objH :: Nil
    // val audioF           = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val duration            = if (duration0 > 0) duration0 else inputSpec.numFrames.toDouble / inputSpec.sampleRate
    val sCfg                = bncCfg.server
    sCfg.nrtOutputPath      = audioF.path
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 1
    sCfg.wireBuffers        = 1024 // higher than default
    sCfg.blockSize          = 64   // keep it compatible to real-time
    sCfg.sampleRate         = inputSpec.sampleRate.toInt
    // bc.init : (S#Tx, Server) => Unit
    bncCfg.span             = Span(0L, (duration * Timeline.SampleRate).toLong)
    val bnc0                = Bounce[I, I].apply(bncCfg)
    // tx.afterCommit {
      bnc0.start()
    // }
    bnc0
  }

  def evaluate[S <: Sys[S]](c: Chromosome[S], algorithm: Algorithm[S], inputSpec: AudioFileSpec, inputExtr: File)
              (implicit tx: S#Tx): Future[Float] = {
    val graph = ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = false, config = true /* false */) // c.graph
    // val cH          = tx.newHandle(c)
    val numVertices = c.vertices.size
    val p           = Promise[Float]()
    tx.afterCommit {
      p.completeWith(evaluateFut(/* cH, */ graph, inputSpec, inputExtr = inputExtr, numVertices = numVertices))
    }
    p.future
  }

  def evaluateBounce(bounce: File, input: File): Future[Float] = {
    val futSpec = TxnExecutor.defaultAtomic { implicit itx =>
      implicit val tx = TxnLike.wrap(itx)
      getInputSpec(input)
    }
    futSpec.map { case (inputExtr, inputSpec) =>
      val fut = eval1(wait = None, bounceF = bounce, inputSpec = inputSpec, inputExtr = inputExtr)
      val res = Await.result(fut, Duration.Inf)
      res.toFloat
    }
  }

  private def evaluateFut(graph: SynthGraph, inputSpec: AudioFileSpec,
                          inputExtr: File, numVertices: Int): Future[Float] = {
    val audioF  = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0    = bounce(graph, audioF = audioF, inputSpec = inputSpec)
    val simFut  = eval1(wait = Some(bnc0), bounceF = audioF, inputSpec = inputSpec, inputExtr = inputExtr)
    val res = simFut.map { sim0 =>
      import numbers.Implicits._
      val pen = Algorithm.vertexPenalty
//      if (sim0 > 0.46) {
//        println(s"DEBUG $audioF")
//      }
      val sim = if (pen <= 0) sim0 else
        sim0 - numVertices.linlin(Algorithm.minNumVertices, Algorithm.maxNumVertices, 0, pen)
      sim.toFloat // new Evaluated(cH, sim)
    }
    res.onComplete { case _ =>
      audioF.delete()
    }
    res
  }

  private def eval1(wait: Option[Processor[Any]], bounceF: File, inputSpec: AudioFileSpec,
                    inputExtr: File): Future[Double] = {
    val bnc = Future {
      wait.foreach { bnc0 =>
        Await.result(bnc0, Duration(4.0, TimeUnit.SECONDS))
      }
      // XXX TODO -- would be faster if we could use a Poll during
      // the bounce and instruct the bounce proc to immediately terminate
      // when seeing a particular message in the console?
      blocking {
        val af = AudioFile.openRead(bounceF)
        try {
          val bufSize = 512
          val b       = af.buffer(bufSize)
          var i       = 0L
          while (i < af.numFrames) {
            val len = math.min(bufSize, af.numFrames - i).toInt
            af.read(b, 0, len)
            var ch = 0
            while (ch < af.numChannels) {
              val bc = b(ch)
              var j = 0
              while (j < len) {
                if (bc(j).isNaN || bc(j).isInfinite) {
                  if (DEBUG) println("Detected NaNs")
                  throw FeatureExtractionFailed(null)
                }
                j += 1
              }
              ch += 1
            }
            i += len
          }
        } finally {
          af.cleanUp()
        }
      }
    }

    val genFolder           = File.createTemp(prefix = "muta_eval", directory = true)
    val genExtr             = genFolder / "gen_feat.xml"

    val normF   = genFolder / Strugatzki.NormalizeName
    if (Algorithm.normalizeCoeffs) {
      if (Algorithm.numCoeffs != featNorms.length + 1)
        throw new IllegalArgumentException(s"Normalize option requires numCoeffs == ${featNorms.length - 1}")
      blocking {
        val normAF  = AudioFile.openWrite(normF, AudioFileSpec(numChannels = featNorms.length, sampleRate = 44100))
        normAF.write(featNorms)
        normAF.close()
      }
    }
    val featF   = File.createTemp(prefix = "gen_feat", suffix = ".aif")

    val ex = bnc.flatMap { _ =>
      val exCfg             = FeatureExtraction.Config()
      exCfg.audioInput      = bounceF
      exCfg.featureOutput   = featF
      exCfg.metaOutput      = Some(genExtr)
      exCfg.numCoeffs       = Algorithm.numCoeffs
      val _ex               = FeatureExtraction(exCfg)
      _ex.start()
      //      _ex.onFailure {
      //        case t => println(s"gen-extr failed with $t")
      //      }
      _ex.recover {
        case cause => throw FeatureExtractionFailed(cause)
      }
    }

    val numFrames = inputSpec.numFrames

    val corr = ex.flatMap { _ =>
      val corrCfg           = FeatureCorrelation.Config()
      corrCfg.metaInput     = inputExtr
      corrCfg.databaseFolder= genFolder
      corrCfg.minSpacing    = Long.MaxValue >> 1
      corrCfg.numMatches    = 1
      corrCfg.numPerFile    = 1
      corrCfg.maxBoost      = Algorithm.maxBoost.toFloat
      corrCfg.normalize     = Algorithm.normalizeCoeffs
      corrCfg.minPunch      = numFrames
      corrCfg.maxPunch      = numFrames
      corrCfg.punchIn       = FeatureCorrelation.Punch(
        span = Span(0L, numFrames),
        temporalWeight = Algorithm.temporalWeight.toFloat)
      val _corr             = FeatureCorrelation(corrCfg)
      _corr.start()
      _corr
    }

    val simFut0 = corr.map { matches =>
      // assert(matches.size == 1)
      val sim0 = matches.headOption.map { m =>
        if (DEBUG) println(m)
        m.sim
      } .getOrElse(0f)
      val sim  = if (sim0.isNaN || sim0.isInfinite) 0.0 else sim0.toDouble
      sim
    }

    val simFut = simFut0.recover {
      case Bounce.ServerFailed(_) => 0.0
      case FeatureExtractionFailed(_) =>
        if (DEBUG) println("Gen-extr failed!")
        0.0

      case _: TimeoutException =>
        if (DEBUG) println("Bounce timeout!")
        wait.foreach { bnc0 => bnc0.abort() }
        0.0    // we aborted the process after 4 seconds
    }

    val res = simFut

    res.onComplete { case _ =>
      if (Algorithm.normalizeCoeffs) normF.delete()
      featF     .delete()
      // audioF    .delete()
      genExtr   .delete()
      genFolder .delete()
    }
    res
  }
}