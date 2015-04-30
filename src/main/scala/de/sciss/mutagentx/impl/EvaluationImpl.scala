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
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.InMemory
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.Span
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction, Strugatzki}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.{Bounce, ExprImplicits, Obj, Proc, Timeline, WorkspaceHandle}
import de.sciss.synth.ugen.SampleRate
import de.sciss.synth.{GE, SynthGraph, UGenSpec, UndefinedRate, ugen}
import de.sciss.{filecache, numbers}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.concurrent.stm.{Txn, TxnExecutor}
import scala.concurrent.{Promise, Await, Future, TimeoutException, blocking}

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

  def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, inputExtr: File,
             duration0: Double = -1): Processor[Any] = {
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

  def mkSynthGraph(c: Chromosome, mono: Boolean, removeNaNs: Boolean)
                  (implicit tx: S#Tx, random: TxnRandom[D#Tx]): SynthGraph = {
    import Util._

    val top = c

    @tailrec def loop(rem: Vec[Vertex], real: Map[Vertex, GE]): Map[Vertex, GE] = rem match {
      case init :+ last =>
        val value: GE = last match {
          case Vertex.Constant(f) => ugen.Constant(f)
          case u @ Vertex.UGen(spec) =>
            val ins = spec.args.map { arg =>
              val res: (AnyRef, Class[_]) = arg.tpe match {
                case UGenSpec.ArgumentType.Int =>
                  val v = arg.defaults.get(UndefinedRate) match {
                    case Some(UGenSpec.ArgumentValue.Int(i)) => i
                    case _ => rrand(1, 2)
                  }
                  (v.asInstanceOf[AnyRef], classOf[Int])

                case UGenSpec.ArgumentType.GE(_, _) =>
                  val inGEOpt = top.edgeMap.get(last.id).getOrElse(Set.empty).flatMap { e =>
                    if (e.inlet == arg.name) real.get(e.targetVertex) else None
                  } .headOption
                  val inGE = inGEOpt.getOrElse {
                    val xOpt = arg.defaults.get(UndefinedRate)
                    val x    = xOpt.getOrElse {
                      val inc = findIncompleteUGenInputs(top, u)
                      println("INCOMPLETE:")
                      inc.foreach(println)
                      sys.error(s"Vertex $spec has no input for inlet $arg")
                    }
                    x match {
                      case UGenSpec.ArgumentValue.Boolean(v)    => ugen.Constant(if (v) 1 else 0)
                      case UGenSpec.ArgumentValue.DoneAction(v) => ugen.Constant(v.id)
                      case UGenSpec.ArgumentValue.Float(v)      => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Inf           => ugen.Constant(Float.PositiveInfinity)
                      case UGenSpec.ArgumentValue.Int(v)        => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Nyquist       => SampleRate.ir / 2
                      case UGenSpec.ArgumentValue.String(v)     => ugen.Escape.stringToGE(v)
                    }
                  }
                  (inGE, classOf[GE])
              }
              res
            }

            u.instantiate(ins)
        }

        loop(init, real + (last -> value))

      case _ =>  real
    }

    SynthGraph {
      import de.sciss.synth.ugen._
      RandSeed.ir()
      val vertices = top.vertices.iterator.toIndexedSeq
      val map   = loop(vertices, Map.empty)
      val ugens = vertices.collect {
        case ugen: Vertex.UGen => ugen
      }
      if (ugens.nonEmpty) {
        val roots = getRoots(top)
        val sig0: GE = if (roots.isEmpty) map(choose(ugens)) else Mix(roots.map(map.apply))
        val sig1  = /* if (mono) */ Mix.mono(sig0) /* else sig0 */
        val sig2  = if (!removeNaNs) sig1 else {
            val isOk = CheckBadValues.ar(sig1, post = 0) sig_== 0
            Gate.ar(sig1, isOk)
          }
        val sig3  = Limiter.ar(LeakDC.ar(sig2))
        val sig   = if (mono) sig3 else Pan2.ar(sig3) // SplayAz.ar(numChannels = 2, in = sig3)
        Out.ar(0, sig)
      }
    }
  }

  def findIncompleteUGenInputs(t1: Chromosome, v: Vertex.UGen)(implicit tx: S#Tx): Vec[String] = {
    val spec      = v.info
    val edgeSet   = t1.edgeMap.get(v.id).getOrElse(Set.empty)
    val argsFree  = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val inc       = argsFree.filterNot(_.defaults.contains(UndefinedRate))
    inc.map(_.name)
  }

  def geArgs(spec: UGenSpec): Vec[UGenSpec.Argument] = {
    val res       = spec.args.filter { arg =>
      arg.tpe match {
        case UGenSpec.ArgumentType.Int => false
        case UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.DoneAction, _) => false
        case _ => true
      }
    }
    res
  }

  private def getRoots(top: Chromosome)(implicit tx: S#Tx): Vec[Vertex.UGen] = {
    val ugens = top.vertices.iterator.collect {
      case ugen: Vertex.UGen => ugen
    }
    val edges = top.edges.iterator.toList
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq
  }

  def evaluate(c: Chromosome, algorithm: Algorithm, inputSpec: AudioFileSpec, inputExtr: File)
              (implicit tx: S#Tx): Future[Evaluated] = {
    import algorithm.global.rng
    val graph       = mkSynthGraph(c, mono = true, removeNaNs = false) // c.graph
    val cH          = tx.newHandle(c)
    val numVertices = c.vertices.size
    val p           = Promise[Evaluated]()
    tx.afterCommit {
      p.completeWith(evaluateFut(cH, graph, inputSpec, inputExtr = inputExtr, numVertices = numVertices))
    }
    p.future
  }

  private def evaluateFut(cH: stm.Source[S#Tx, Chromosome], graph: SynthGraph, inputSpec: AudioFileSpec,
                          inputExtr: File, numVertices: Int): Future[Evaluated] = {
    val audioF = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0 = bounce(graph, audioF = audioF, inputSpec = inputSpec, inputExtr = inputExtr)
    val bnc = Future {
      Await.result(bnc0, Duration(4.0, TimeUnit.SECONDS))
      // XXX TODO -- would be faster if we could use a Poll during
      // the bounce and instruct the bounce proc to immediately terminate
      // when seeing a particular message in the console?
      blocking {
        val af = AudioFile.openRead(audioF)
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
    //    bnc.onFailure {
    //      case t => println(s"bnc failed with $t")
    //    }

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
      exCfg.audioInput      = audioF
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
        bnc0.abort()
        0.0    // we aborted the process after 4 seconds
    }

    val res = simFut.map { sim0 =>
      import numbers.Implicits._
      val pen = Algorithm.vertexPenalty
      val sim = if (pen <= 0) sim0 else
        sim0 - numVertices.linlin(Algorithm.minNumVertices, Algorithm.maxNumVertices, 0, pen)
      new Evaluated(cH, sim)
    }

    res.onComplete { case _ =>
      if (Algorithm.normalizeCoeffs) normF.delete()
      featF     .delete()
      audioF    .delete()
      genExtr   .delete()
      genFolder .delete()
    }
    res
  }
}