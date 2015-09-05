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
import java.{util => ju}

import de.sciss.file._
import de.sciss.filecache.{TxnConsumer, TxnProducer}
import de.sciss.lucre.stm.{Sys, TxnLike}
import de.sciss.lucre.synth.InMemory
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.Span
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction, Strugatzki}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.{Bounce, Proc, Timeline, WorkspaceHandle}
import de.sciss.synth.ugen.{BinaryOpUGen, ConfigOut, Constant, Mix, RandSeed, UnaryOpUGen}
import de.sciss.synth.{GE, SynthGraph}
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

  def getInputSpec(config: Algorithm.Config, input: File)(implicit tx: TxnLike): Future[(File, AudioFileSpec)] = {
    import config._
    val key       = input -> numMFCC
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

    // val exp = ExprImplicits[I]

    val objH = inMemory.step { implicit tx =>
      val proc      = Proc[I]
      proc.graph()  = graph
      // val procObj   = Obj(Proc.Elem(proc))
      tx.newHandle(proc) // (Obj.typedSerializer[I, Proc.Elem[I]])
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

  def evaluate[S <: Sys[S]](config: Algorithm.Config, c: Chromosome[S], inputSpec: AudioFileSpec, inputExtr: File)
              (implicit tx: S#Tx): (SynthGraph, Future[Float]) = {
    val graph = MkSynthGraph(c, mono = true, removeNaNs = false, config = true, ranges = true)
    // val cH          = tx.newHandle(c)
    val numVertices = c.vertices.size
    val p           = Promise[Float]()
    tx.afterCommit {
      p.completeWith(evaluateFut(config, graph, inputSpec, inputExtr = inputExtr, numVertices = numVertices))
    }
    (graph, p.future)
  }

  def evaluateBounce(config: Algorithm.Config, bounce: File, input: File): Future[Float] = {
    val futSpec = TxnExecutor.defaultAtomic { implicit itx =>
      implicit val tx = TxnLike.wrap(itx)
      getInputSpec(config, input)
    }
    futSpec.map { case (inputExtr, inputSpec) =>
      val fut = eval1(config, wait = None, bounceF = bounce, inputSpec = inputSpec, inputExtr = inputExtr)
      val res = Await.result(fut, Duration.Inf)
      res.toFloat
    }
  }

  private def evaluateFut(config: Algorithm.Config, graph: SynthGraph, inputSpec: AudioFileSpec,
                          inputExtr: File, numVertices: Int): Future[Float] = {
    import config._
    val audioF  = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0    = bounce(graph, audioF = audioF, inputSpec = inputSpec)
    val simFut  = eval1(config, wait = Some(bnc0), bounceF = audioF, inputSpec = inputSpec, inputExtr = inputExtr)
    val res = simFut.map { sim0 =>
      import numbers.Implicits._
      val pen = vertexPenalty
//      if (sim0 > 0.46) {
//        println(s"DEBUG $audioF")
//      }
      val sim = if (pen <= 0) sim0 else
        sim0 - numVertices.linlin(minNumVertices, maxNumVertices, 0, pen)
      sim.toFloat // new Evaluated(cH, sim)
    }
    res.onComplete { case _ =>
      audioF.delete()
    }
    res
  }

  private def eval1(config: Algorithm.Config, wait: Option[Processor[Any]], bounceF: File, inputSpec: AudioFileSpec,
                    inputExtr: File): Future[Double] = {
    import config.{wait => _, _}
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
    if (normalizeMFCC) {
      if (numMFCC != featNorms.length + 1)
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
      exCfg.numCoeffs       = numMFCC
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
      corrCfg.maxBoost      = maxBoost.toFloat
      corrCfg.normalize     = normalizeMFCC
      corrCfg.minPunch      = numFrames
      corrCfg.maxPunch      = numFrames
      corrCfg.punchIn       = FeatureCorrelation.Punch(
        span = Span(0L, numFrames),
        temporalWeight = temporalWeight.toFloat)
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
      if (normalizeMFCC) normF.delete()
      featF     .delete()
      // audioF    .delete()
      genExtr   .delete()
      genFolder .delete()
    }
    res
  }

  private def mkSimGraph(g: SynthGraph): NikolicSimilarity.Graph[Any] = {
    val res   = NikolicSimilarity.Graph[Any]
    val map   = new ju.IdentityHashMap[Product, Any]
    var users = Set.empty[Any]

    def uniqueString(s: String) =
      if (!users.contains(s)) s else {
        var cnt = 0
        while (users.contains(s"$s$cnt")) cnt += 1
        s"$s$cnt"
      }

    def addSource(p: Product): Any = {
      val n0 = map.get(p)
      if (n0 != null) n0 else {
        val userObject = p match {
          case Constant(f) => f
          case bin: BinaryOpUGen => uniqueString(bin.selector.name)
          case un : UnaryOpUGen  => uniqueString(un .selector.name)
          case x                 => uniqueString(x.productPrefix)
        }
        val n1 = userObject // new GraphNode(userObject)
        res.addVertex(n1)
        map.put(p, n1)
        users += userObject
        p.productIterator.foreach {
          case arg: GE =>
            val n2 = addSource(arg)
            res.addEdge(n2, n1)
            // res.addEdge(n1, n2)
          case _ =>
        }
        n1
      }
    }

    g.sources.foreach {
      case _: RandSeed | _: ConfigOut | _: Mix | _: Mix.Mono =>
      case x => addSource(x)
    }
    res
  }

  def graphSimilarity(a: SynthGraph, b: SynthGraph): Float = {
    val ng1 = mkSimGraph(a)
    val ng2 = mkSimGraph(b)
    NikolicSimilarity(ng1, ng2).toFloat
  }

//  def graphSimilarity(a: SynthGraph, b: SynthGraph): Float = {
//    val acc1  = new SynthGraphAccessor(a)
//    val acc2  = new SynthGraphAccessor(b)
////    val m0    = new GraphIsomorphism(acc1, acc2)
////    val m3    = new SubgraphIsomorphism(acc1, acc2,
////      "AlwaysTrue", // SubgraphIsomorphism.DEFAULT_NODE_LABEL_SIMILARITY_MEASURE,
////      5, // SubgraphIsomorphism.DEFAULT_MIN_CLIQUE_SIZE,
////      0.0, // SubgraphIsomorphism.DEFAULT_LABEL_WEIGHT,
////      1.0, // SubgraphIsomorphism.DEFAULT_STRUCTURE_WEIGHT,
////      SubgraphIsomorphism.DEFAULT_DENOMINATOR,
////      SubgraphIsomorphism.NODE_GROUPING
////    )
//    val m3 = new SubgraphIsomorphism(acc1, acc2,
//      SubgraphIsomorphism.DEFAULT_NODE_LABEL_SIMILARITY_MEASURE,
//      4, // SubgraphIsomorphism.DEFAULT_MIN_CLIQUE_SIZE,
//      SubgraphIsomorphism.DEFAULT_LABEL_WEIGHT,
//      SubgraphIsomorphism.DEFAULT_STRUCTURE_WEIGHT,
//      SubgraphIsomorphism.DEFAULT_DENOMINATOR,
//      SubgraphIsomorphism.NODE_GROUPING
//    )
////    val sim0 = { m0.calculate(); m0.getGraphIsomorphism.toFloat }
////    val sim1 = if (sim0 == 1) sim0 else { m1.calculate(); m1.getSimilarity.toFloat }
//
////    val m2 = new MaxGraphIsoCovering(acc1, acc2,
////      SubgraphIsomorphism.DEFAULT_NODE_LABEL_SIMILARITY_MEASURE,
////      SubgraphIsomorphism.DEFAULT_MIN_CLIQUE_SIZE,
////      SubgraphIsomorphism.DEFAULT_LABEL_WEIGHT,
////      SubgraphIsomorphism.DEFAULT_STRUCTURE_WEIGHT,
////      SubgraphIsomorphism.DEFAULT_DENOMINATOR,
////      SubgraphIsomorphism.NODE_GROUPING,
////      MaxGraphIsoCovering.DEFAULT_COVERING
////    )
////    m2.calculate()
////    require(m2.isCalculated)
////    m2.getSimilarity.toFloat
//
////    val m3 = new MaxCommonSubgraphIsoValiente(acc1, acc2,
////      1, // MaxCommonSubgraphIsoValiente.DEFAULT_MIN_CLIQUE_SIZE,
////      MaxCommonSubgraphIsoValiente.DEFAULT_STRUCTURE_WEIGHT,
////      MaxCommonSubgraphIsoValiente.DEFAULT_LABEL_WEIGHT,
////      MaxCommonSubgraphIsoValiente.DEFAULT_DENOMINATOR
////    )
//    m3.calculate()
//    require(m3.isCalculated)
//    m3.getSimilarity.toFloat
//  }

//  private final class SynthGraphAccessor(g: SynthGraph) extends AbstractGraphAccessor {
//    populate()
//    validate()
//
//    override def toString = g.toString
//
//    private def validate(): Unit = {
//      import JavaConversions._
//      nodeSet.foreach { n =>
//        val numPre  = n.getPredecessorSet.size()
//        val numSucc = n.getSuccessorSet  .size()
//        val numAdj  = n.getAdjacentSet   .size()
//        val inDeg   = n.getInDegree
//        val outDeg  = n.getOutDegree
//        assert(numPre + numSucc == numAdj, s"Node $n has $numPre predecessors, $numSucc successors, that do not sum up to $numAdj adjacents")
//        assert(numPre  == inDeg , s"Node $n has $numPre predecessors but in-degree reported as $inDeg")
//        assert(numSucc == outDeg, s"Node $n has $numSucc successors but out-degree reported as $outDeg")
//      }
//    }
//
//    private def populate(): Unit = {
//      val map   = new ju.IdentityHashMap[Product, IGraphNode]
//      var users = Set.empty[Any]
//
//      def uniqueString(s: String) =
//        if (!users.contains(s)) s else {
//          var cnt = 0
//          while (users.contains(s"$s$cnt")) cnt += 1
//          s"$s$cnt"
//        }
//
//      def addSource(p: Product): IGraphNode = {
//        val n0 = map.get(p)
//        if (n0 != null) n0 else {
//          val userObject = p match {
//            case Constant(f) => f
//            case bin: BinaryOpUGen => uniqueString(bin.selector.name)
//            case un : UnaryOpUGen  => uniqueString(un .selector.name)
//            case x                 => uniqueString(x.productPrefix)
//          }
//          val n1 = new GraphNode(userObject)
//          addNode(n1)
//          map.put(p, n1)
//          users += userObject
//          p.productIterator.foreach {
//            case arg: GE =>
//              val n2 = addSource(arg)
//              setEdge(n2, n1)
//              // setEdge(n1, n2)
//            case _ =>
//          }
//          n1
//        }
//      }
//
//      g.sources.foreach {
//        case _: RandSeed | _: ConfigOut | _: Mix | _: Mix.Mono =>
//        case x => addSource(x)
//      }
//    }
//
//    def getMaximumDirectedPathLength: Double = ...
//
//    def getMostRecentCommonAncestor(nodeA: IGraphNode, nodeB: IGraphNode): IGraphNode = ...
//
//    def getSuccessors(node: IGraphNode, direct: Boolean): ju.Set[IGraphNode] =
//      if (direct) node.getSuccessorSet else ...
//
//    def getPredecessors(node: IGraphNode, direct: Boolean): ju.Set[IGraphNode] =
//      if (direct) node.getPredecessorSet else ...
//
//    def getMaxDepth: Double = ...
//
//    def getShortestPath(nodeA: IGraphNode, nodeB: IGraphNode): Double = ...
//  }
}