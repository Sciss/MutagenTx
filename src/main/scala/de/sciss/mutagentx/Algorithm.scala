/*
 *  Algorithm.scala
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

import de.sciss.file.File
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{DataStore, DataStoreFactory}
import de.sciss.lucre.{confluent, data, stm}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.Serializer
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.Confluent
import de.sciss.synth.{UGenSpec, UndefinedRate}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor
import scala.concurrent.{Await, ExecutionContext, Future, Promise, blocking}
import scala.language.higherKinds

object Algorithm {
  val DEBUG = false

  // ---- generation ----
  val population      : Int     = 1000
  val constProb       : Double  = 0.5
  val minNumVertices  : Int     = 30
  val maxNumVertices  : Int     = 100
  val nonDefaultProb  : Double  = 0.99 // 0.5

  // ---- evaluation ----
  val numCoeffs       : Int     = 42
  val normalizeCoeffs : Boolean = false // true
  val maxBoost        : Double  = 10.0
  val temporalWeight  : Double  = 0.3
  val vertexPenalty   : Double  = 0.01

  // ---- breeding ----
  val selectionFrac   : Double  = 0.33
  val numElitism      : Int     = 5
  val mutMin          : Int     = 2
  val mutMax          : Int     = 4
  val mutationProb    : Double  = 0.5 // 0.75

  implicit val executionContext: ExecutionContext = {
    ExecutionContext.Implicits.global
    // SoundProcesses.executionContext
    // val ex = Executors.newFixedThreadPool(6)
    // ExecutionContext.fromExecutor(ex)
  }

  def tmpConfluent(input: File): Confluent =
    impl.ConfluentAlgorithm.tmp(input)

  def confluent(dir: File, input: File): Confluent =
    impl.ConfluentAlgorithm.apply(dir = dir, input = input)

  trait Confluent extends Algorithm[ConfluentReactive] {
    type Global = GlobalState.Confluent
  }
}
trait Algorithm[S <: Sys[S]] { algo =>
  import Algorithm.{DEBUG, constProb, maxNumVertices, minNumVertices, mutationProb, numElitism, selectionFrac}
  import Util.{choose, coin, exprand, rrand}

  type C = Chromosome[S]

  def genome(implicit tx: S#Tx): Genome[S]

  // implicit def chromoSer: Serializer[S#Tx, S#Acc, Chromosome[S]]

  /** Input target sound file. */
  def input: File

  /** Target sound's feature extraction file. */
  def inputExtr: File

  /** Target sound's specification. */
  def inputSpec: AudioFileSpec

  def system: S

  type Global <: GlobalState[S]

  val global: Global

  implicit def ord: data.Ordering[S#Tx, Vertex[S]]

  import global.rng

  /** Creates the initial population of size `n`. */
  def init(n: Int)(implicit tx: S#Tx): Unit =
    genome.chromosomes() = Vector.fill(n)(mkIndividual())

  /** Creates an individual chromosome. */
  def mkIndividual()(implicit tx: S#Tx): C = {
    val num = rrand(minNumVertices, maxNumVertices)
    val res = Topology.empty[S, Vertex[S], Edge[S]]
    for (i <- 0 until num) addVertex(res)
    res // new Chromosome(t0, seed = random.nextLong())
  }

  /** Adds a random vertex to an existing chromosome. */
  def addVertex(c: C)(implicit tx: S#Tx): Unit = {
    if (coin(constProb)) {
      val v = mkConstant()
      if (DEBUG) println(s"addVertex($v)")
      c.addVertex(v)

    } else {
      val v = mkUGen()
      c.addVertex(v)
      if (DEBUG) println(s"addVertex($v)")
      completeUGenInputs(c, v)
    }
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

  /** Adds vertices a vertex `v` newly added to a chromosome `c` to ensure
    * it is fully wired.
    */
  def completeUGenInputs(c: C, v: Vertex.UGen[S])(implicit tx: S#Tx): Unit = {
    import Algorithm.nonDefaultProb
    val spec    = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeSet = c.edgeMap.get(v).getOrElse(Set.empty)
    val argsFree = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val (hasDef, hasNoDef)          = argsFree.partition(_.defaults.contains(UndefinedRate))
    val (useNotDef, _ /* useDef */) = hasDef.partition(_ => coin(nonDefaultProb))
    val findDef = hasNoDef ++ useNotDef

    @tailrec def loopVertex(rem: Vec[UGenSpec.Argument]): Unit = rem match {
      case head +: tail =>
        val options = c.vertices.iterator.filter { vi =>
          val e = Edge.make[S](v, vi, head.name)
          c.canAddEdge(e)
        }
        if (options.nonEmpty) {
          val vi  = choose(options.toIndexedSeq)
          val e   = Edge.make[S](v, vi, head.name)
          if (DEBUG) println(s"addEdge($e)")
          c.addEdge(e) // .get // ._1
        } else {
          val vi  = mkConstant()
          if (DEBUG) println(s"addVertex($vi)")
          c.addVertex(vi)
          val e   = Edge.make[S](v, vi, head.name)
          if (DEBUG) println(s"addEdge($e)")
          c.addEdge(e) // .get
        }

        loopVertex(tail)

      case _ =>
    }

    loopVertex(findDef)
  }

  /** Creates a random constant vertex. */
  def mkConstant()(implicit tx: S#Tx): Vertex.Constant[S] = {
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    val v   = Vertex.Constant(f.toFloat)
    v
  }

  /** Creates a random UGen vertex. */
  def mkUGen()(implicit tx: S#Tx): Vertex.UGen[S] = {
    val spec    = choose(UGens.seq)
    val v       = Vertex.UGen(spec)
    v
  }

  /** Runs the evaluation on all chromosomes of the current genome,
    * returning the fitness vector (the genome is not updated).
    */
  def evaluate()(implicit tx: S#Tx): Processor[Vec[Float]] = mkEvaluator(updateGenome = false)(identity)

  /** Runs the evaluation on all chromosomes of the current genome,
    * updating the genome's fitness vector.
    */
  def evaluateAndUpdate()(implicit tx: S#Tx): Processor[Unit] = mkEvaluator(updateGenome = true)(_ => ())

  private def mkEvaluator[A](updateGenome: Boolean)(map: Vec[Float] => A)(implicit tx: S#Tx): Processor[A] = {
    val c   = genome.chromosomes().map(tx.newHandle(_))
    val proc  = new Evaluator(c, updateGenome = updateGenome, map = map)
    import Algorithm.executionContext
    tx.afterCommit(proc.start())
    proc
  }

  private[this] final class Evaluator[A](c: Vec[stm.Source[S#Tx, Chromosome[S]]], updateGenome: Boolean,
                                         map: Vec[Float] => A)
    extends ProcessorImpl[A, Processor[A]] with Processor[A] {

    def body(): A = {
      val clumps    = c.grouped(4).toIndexedSeq
      val numClumps = clumps.size
      val fit = clumps.zipWithIndex.flatMap { case (clump, ci) =>
        val futClump: Vec[Future[Float]] = global.cursor.step { implicit tx =>
          clump.map { cH =>
            impl.EvaluationImpl.evaluate(cH(), algo, inputSpec, inputExtr)
          }
        }
        val clumpRes = Await.result(Future.sequence(futClump), Duration.Inf)
        progress = (ci + 1).toDouble / numClumps
        checkAborted()
        clumpRes
      }

      if (updateGenome)
        global.cursor.step { implicit tx =>
          genome.fitness() = fit
        }

      map(fit)
    }
  }

  /** Runs the selection stage of the algorithm, using `all` inputs which
    * are chromosomes paired with their fitnesses.
    */
  def select(all: Vec[(Chromosome[S], Float)])(implicit tx: S#Tx): Set[Chromosome[S]] = {
    val pop   = all.size
    val n     = (pop * selectionFrac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[(Chromosome[S], Float)], out: Set[Chromosome[S]]): Set[Chromosome[S]] =
      if (rem == 0) out else {
        val sum     = in.view.map(_._2).sum
        val rem1    = rem - 1
        if (sum == 0.0) {
          val chosen = in.head
          loop(rem1, in - chosen, out + chosen._1)
        } else {
          val inIdx       = in.zipWithIndex[(Chromosome[S], Float), Vec[((Chromosome[S], Float), Int)]](breakOut)
          val norm        = inIdx.map {
            case ((c, f), j) => (j, f / sum)
          }
          val sorted      = norm.sortBy(_._2)
          val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
          val roulette    = rng.nextDouble()
          val idxS        = acc.indexWhere(_ > roulette)
          val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
          val (chosen, _) = inIdx(idx)
          loop(rem1, in - chosen, out + chosen._1)
        }
      }

    val sel   = loop(n, all.toSet.filterNot { case (_, f) => f.isInfinity|| f.isNaN }, Set.empty)
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  /** Selects the best matching chromosomes. */
  def elitism(all: Vec[(Chromosome[S], Float)])(implicit tx: S#Tx): Vec[Chromosome[S]] =
    if (numElitism == 0) Vector.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = all.sortBy(-_._2)
      val res = Vector.newBuilder[Chromosome[S]]
      res.sizeHint(numElitism)
      val it = eliteCandidates.iterator
      var sz = 0
      var fl = Double.NaN
      while (sz < numElitism && it.hasNext) {
        val (c, f) = it.next()
        if (f != fl) {
          res += c
          sz  += 1
          fl   = f
        }
      }
      res.result()
    }

  /** Performs one iteration of the algorithm, assuming that current population
    * was already evaluated. Steps:
    *
    * - elitism
    * - selection
    * - breeding: mutation and cross-over
    * - evaluation
    */
  def iterate(): Processor[Unit]
}