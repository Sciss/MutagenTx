/*
 *  AlgorithmImpl.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.{SynthGraph, UGenSpec, UndefinedRate}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}

trait AlgorithmImpl[S <: Sys[S]] extends Algorithm[S] { algo =>
  import Algorithm.DEBUG
  import config.generation._
  import config.penalty._
  import config.breeding._
  import Util.{choose, coin, exprand, rrand}
  import global.rng

  /** Creates the initial population of size `n`. */
  def initialize()(implicit tx: S#Tx): Processor[Unit] = {
    import Algorithm.executionContext
    val res = new Initializer(config.generation.population)
    res.start()
    res
  }

  protected def ephemeral: Boolean

  private[this] final class Initializer(n: Int)
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = blocking {
      val cursor = global.cursor
      if (ephemeral) {
        val sq = Vector.tabulate(n) { i =>
          val res = cursor.step { implicit tx => mkIndividual() }
          progress = ((i + 1).toDouble / n) * 0.8
          res
        }
        cursor.step { implicit tx =>
          genome.chromosomes() = sq
        }
      } else {
        cursor.step { implicit tx =>
          val sq = Vector.fill(n)(mkIndividual())
          genome.chromosomes() = sq
        }
      }

      progress = 1.0
      checkAborted()
    }
  }

  /** Creates an individual chromosome. */
  def mkIndividual()(implicit tx: S#Tx): C = {
    val num = rrand(minNumVertices, maxNumVertices)
    val res = Chromosome.empty[S]
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

  /** Utility method that collects the general arguments into which other `GE` elements may be plugged. */
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
    val spec    = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeSet = c.targets(v) // .getOrElse(Set.empty)
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
    val c     = genome.chromosomes().map(tx.newHandle(_))
    val proc  = new Evaluator(c, updateGenome = updateGenome, map = map)
    import Algorithm.executionContext
    tx.afterCommit(proc.start())
    proc
  }

  private[this] final class Evaluator[A](c: Vec[stm.Source[S#Tx, C]], updateGenome: Boolean,
                                         map: Vec[Float] => A)
    extends ProcessorImpl[A, Processor[A]] with Processor[A] {

    def body(): A = {
      val clumps      = c.grouped(4).toIndexedSeq
      val numClumps   = clumps.size

      val nextIter    = global.cursor.step { implicit tx => global.numIterations() + 1 }
      val graphPen    = graphPenaltyIter > 0 && (nextIter % graphPenaltyIter == 0)
      val progWeight  = 1.0 / (if (graphPen) numClumps << 1 else numClumps)

      val (graphs, fit0) = clumps.zipWithIndex.flatMap { case (clump, ci) =>
        val (graphs0: Vec[SynthGraph], futClump: Vec[Future[Float]]) = global.cursor.step { implicit tx =>
          clump.map { cH =>
            impl.EvaluationImpl.evaluate(config, cH(), inputSpec, inputExtr)
          }
        } .unzip
        val clumpRes = Await.result(Future.sequence(futClump), Duration.Inf)
        progress = (ci + 1) * progWeight
        checkAborted()
        graphs0 zip clumpRes
      } .unzip

      val fit = if (!graphPen) fit0 else {
        println("---- begin graph sim ----")
        val N = graphs.size
        val sims = graphs.zipWithIndex.map { case (a, ai) =>
          val (simSum, simNum) = ((0.0, 0) /: graphs) { case (old @ (sum, num), b) =>
            if ((a ne b) && coin1(graphPenaltyCoin))
              (sum + EvaluationImpl.graphSimilarity(a, b), num + 1)
            else old
          }
          progress = (ai + 1).toDouble / N
          checkAborted()
          simSum / simNum
        }
        val minSim = 0.0 // sims.min
        val maxSim = graphPenaltyCeil // sims.max
        val fit1 = (fit0 zip sims).map { case (f, sim) =>
          import numbers.Implicits._
          val pen = sim.linlin(minSim, maxSim, 0.0, graphPenaltyAmt)
          f * math.max(0.0f, (1.0 - pen).toFloat)
        }

        println("---- end   graph sim ----")
        fit1
      }

      if (updateGenome)
        global.cursor.step { implicit tx =>
          genome.fitness() = fit
          global.numIterations() = nextIter
        }

      map(fit)
    }
  }

  // XXX TODO --- should also use transactional coin here
  private[this] def coin1(p: Double): Boolean = math.random < p

  /** Runs the selection stage of the algorithm, using `all` inputs which
    * are chromosomes paired with their fitness values.
    */
  def select(all: Vec[(C, Float)])(implicit tx: S#Tx): Set[C] = {
    val pop   = all.size
    val n     = (pop * selectionFrac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[(C, Float)], out: Set[C]): Set[C] =
      if (rem == 0) out else {
        val sum     = in.view.map(_._2).sum
        val rem1    = rem - 1
        if (sum == 0.0) {
          val chosen = in.head
          loop(rem1, in - chosen, out + chosen._1)
        } else {
          val inIdx       = in.zipWithIndex[(C, Float), Vec[((C, Float), Int)]](breakOut)
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
  def elitism(all: Vec[(C, Float)])(implicit tx: S#Tx): Vec[C] =
    if (numElitism == 0) Vector.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = all.sortBy(-_._2)
      val res = Vector.newBuilder[C]
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
}
