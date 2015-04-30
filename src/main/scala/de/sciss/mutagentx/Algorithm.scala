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

import java.util.concurrent.{Executors, TimeUnit}

import de.sciss.file.File
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{DataStore, DataStoreFactory}
import de.sciss.lucre.{confluent, stm}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{SoundProcesses, Confluent}
import de.sciss.synth.{UGenSpec, UndefinedRate}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds

object Algorithm {
  val DEBUG = false

  // ---- generation ----
  val constProb       : Double = 0.5
  val minNumVertices  : Int    = 4
  val maxNumVertices  : Int    = 50
  val nonDefaultProb  : Double = 0.9 // 0.5

  // ---- evaluation ----
  val numCoeffs       : Int     = 13
  val normalizeCoeffs : Boolean = false // true
  val maxBoost        : Double  = 8.0
  val temporalWeight  : Double  = 0.5
  val vertexPenalty   : Double  = 0.2

  implicit val executionContext: ExecutionContext = {
    ExecutionContext.Implicits.global
    // SoundProcesses.executionContext
    //    val ex = Executors.newFixedThreadPool(2)
    //    ExecutionContext.fromExecutor(ex)
  }

  def tmp(input: File): Algorithm = {
    val cfg = BerkeleyDB.Config()
    // cfg.lockTimeout = Duration(2000, TimeUnit.MILLISECONDS)
    val dbf = BerkeleyDB.tmp(cfg)
    create(dbf, input)
  }

  def apply(dir: File, input: File): Algorithm = {
    val dbf = BerkeleyDB.factory(dir)
    create(dbf, input)
  }

  private def create(dbf: DataStoreFactory[DataStore], _input: File): Algorithm = {
    val futInput = TxnExecutor.defaultAtomic { implicit tx =>
      impl.EvaluationImpl.getInputSpec(_input)
    }
    val (_inputExtr, _inputSpec) = Await.result(futInput, Duration.Inf)

    new Algorithm {
      implicit val system = Confluent(dbf)
      val (handle, global) = system.rootWithDurable { implicit tx =>
        implicit val dtx = system.durableTx(tx)
        Genome.empty
      } { implicit tx =>
        GlobalState()
      }

      def genome(implicit tx: S#Tx): Genome = handle()

      val input     = _input
      val inputExtr = _inputExtr
      val inputSpec = _inputSpec
    }
  }
}
trait Algorithm {
  import Algorithm.{DEBUG, constProb, maxNumVertices, minNumVertices}
  import Util.{choose, coin, exprand, rrand}

  def genome(implicit tx: S#Tx): Genome

  def input: File
  def inputExtr: File
  def inputSpec: AudioFileSpec

  def system: S

  val global: GlobalState

  import global.rng

  private val mutationProb  = 0.5
  private val selectionFrac = 0.25
  private val numElitism = 0 // 2

  def init(n: Int)(implicit tx: S#Tx): Unit =
    genome.chromosomes() = Vector.fill(n)(mkIndividual())

  def mkIndividual()(implicit tx: S#Tx): Chromosome = {
    val num = rrand(minNumVertices, maxNumVertices)
    val res = Topology.empty[Vertex, Edge]
    for (i <- 0 until num) addVertex(res)
    res // new Chromosome(t0, seed = random.nextLong())
  }

  def addVertex(c: Chromosome)(implicit tx: S#Tx): Unit = {
    if (coin(constProb)) {
      val v = mkConstant()
      if (DEBUG) println(s"addVertex($v)")
      c.addVertex(v)

    } else {
      val v   = mkUGen()
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

  def completeUGenInputs(c: Chromosome, v: Vertex.UGen)(implicit tx: S#Tx): Unit = {
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
          val e = Edge(v, vi, head.name)
          c.canAddEdge(e)
        }
        if (options.nonEmpty) {
          val vi  = choose(options.toIndexedSeq)
          val e   = Edge(v, vi, head.name)
          if (DEBUG) println(s"addEdge($e)")
          c.addEdge(e).get // ._1
        } else {
          val vi  = mkConstant()
          if (DEBUG) println(s"addVertex($vi)")
          c.addVertex(vi)
          val e   = Edge(v, vi, head.name)
          if (DEBUG) println(s"addEdge($e)")
          c.addEdge(e).get
        }

        loopVertex(tail)

      case _ =>
    }

    loopVertex(findDef)
  }

  def mkConstant()(implicit tx: S#Tx): Vertex.Constant = {
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    val v   = Vertex.Constant(f.toFloat)
    v
  }

  def mkUGen()(implicit tx: S#Tx): Vertex.UGen =  {
    val spec    = choose(UGens.seq)
    val v       = Vertex.UGen(spec)
    v
  }

  //  def evaluate()(implicit tx: S#Tx): Future[Vec[Evaluated]] = {
  //    val futs = genome.chromosomes().map { c =>
  //      impl.EvaluationImpl(c, this)(tx, global.cursor)
  //    }
  //    import Algorithm.executionContext
  //    Future.sequence(futs)
  //  }

  def evaluate()(implicit tx: S#Tx): Future[Vec[Evaluated]] = {
    val futs = genome.chromosomes().map { c =>
      impl.EvaluationImpl.evaluate(c, this, inputSpec, inputExtr)
    }
    import Algorithm.executionContext
    Future.sequence(futs)
  }

//  def evaluate()(implicit tx: S#Tx): Unit =
//    genome.chromosomes().foreach { cH =>
//      val b = cH.bits
//      val h = b.size / 2
//      // simple example function: lhs should be true, rhs should be false
//      val c = b.zipWithIndex.count { case (v, i) => v == (i < h) }
//      cH.fitness() = c.toDouble / b.size
//    }

  def print()(implicit tx: S#Tx): Unit = {
    val s = mkString()
    tx.afterCommit(println(s))
  }

  def mkString()(implicit tx: S#Tx): String = ???
//    genome.chromosomes().zipWithIndex.map { case (cH, i) =>
//      val b  = cH.bits.map { v => if (v) '1' else '0' } .mkString
//      val s0 = f"${i + 1}%2d  ${cH.fitness()}%1.3f  $b"
//      if (DEBUG) s"$s0  ${cH.debugString}" else s0
//    } .mkString("\n")

  def select(all: Vec[Chromosome])(implicit tx: S#Tx): Set[Chromosome] = {
    ???
//    implicit val dtx = tx.durable
//
//    val pop   = all.size
//    val n     = (pop * selectionFrac + 0.5).toInt
//
//    @tailrec def loop(rem: Int, in: Set[Chromosome], out: Set[Chromosome]): Set[Chromosome] = if (rem == 0) out else {
//      val sum     = in.view.map(_.fitness()).sum
//      val rem1    = rem - 1
//      if (sum == 0.0) {
//        val chosen = in.head
//        loop(rem1, in - chosen, out + chosen)
//      } else {
//        val inIdx       = in.zipWithIndex[Chromosome, Vec[(Chromosome, Int)]](breakOut)
//        val norm        = inIdx.map {
//          case (c, j) => (j, c.fitness() / sum)
//        }
//        val sorted      = norm.sortBy(_._2)
//        val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
//        val roulette    = rng.nextDouble()
//        val idxS        = acc.indexWhere(_ > roulette)
//        val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
//        val (chosen, _) = inIdx(idx)
//        loop(rem1, in - chosen, out + chosen)
//      }
//    }
//
//    val sel   = loop(n, all.toSet, Set.empty)
//    // val remove  = all -- sel
//    // remove.foreach(prev.remove)
//    sel
  }

  def elitism(all: Vec[Chromosome])(implicit tx: S#Tx): Vec[Chromosome] = {
    ???
//    val sel = all.sortBy(-_.fitness()).take(numElitism)
//    sel
  }

  def scramble[A, CC[~] <: IndexedSeq[~], To](in: CC[A])(implicit tx: S#Tx, random: TxnRandom.Persistent[D],
                                                         cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: IndexedSeq[A]
    implicit val dtx = tx.durable
    while (rem.nonEmpty) {
      val idx = random.nextInt(rem.size)
      val e = rem(idx)
      rem = rem.patch(idx, Nil, 1)
      b += e
    }
    b.result()
  }

  /** Produces a sequence of `n` items by crossing each two parents from the input `sel` selection.
    *
    * It assumes the invoking transaction is 'up-to-date' and will cause
    * the selection's cursors to step from this transaction's input access.
    */
  def crossover(sq: Vec[stm.Source[S#Tx, Chromosome]], n: Int,
                inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, Chromosome])] = {
    ???
//    var res = Vector.empty[(S#Acc, confluent.Source[S, Chromosome])]
//    while (res.size < n) {
//      val idx0      = res.size << 1
//      val chosen0H  = sq( idx0      % sq.size)
//      val chosen1H  = sq((idx0 + 1) % sq.size)
//      val csr       = global.forkCursor
//      val hs0 = csr.stepFrom(inputAccess) { implicit tx =>
//        implicit val dtx = tx.durable
//        val chosen0 = chosen0H()
//        val chosen1 = chosen1H()
//        val numBits = chosen0.size
//        require(numBits > 1)
//        val split   = rng.nextInt(numBits - 1) + 1
//
//        @tailrec def loop(rem: Int, next0: S#Var[Option[Bit]], next1: S#Var[Option[Bit]]): (S#Var[Option[Bit]], S#Var[Option[Bit]]) =
//          if (rem == 0) (next0, next1) else {
//            val nn0 = next0().get
//            val nn1 = next1().get
//            loop(rem - 1, nn0.next, nn1.next)
//          }
//
//        val (p0, p1) = loop(split, chosen0.head, chosen1.head)
//        val p0v = p0()
//        val p1v = p1()
//
//        if (DEBUG) println(s"cross ($chosen0, $chosen1) at $split")
//
//        val _res0 = {
//          p0() = p1v
//          // if (DEBUG) println(s"$p0() = $p1v")
//          tx.newHandle(chosen0)
//        }
//        val _res1 = if (res.size + 1 == n) _res0 :: Nil else {
//          p1() = p0v
//          // if (DEBUG) println(s"$p1() = $p0v")
//          _res0 :: tx.newHandle(chosen1) :: Nil
//        }
//
//        _res1
//      }
//      val hs  = hs0 // csr.step { implicit tx => hs0.map(h => tx.newHandle(h())) }
//      val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
//      if (DEBUG) println(s"$hs - $pos")
//      hs.foreach(h => res :+= (pos, h))
//    }
//    res
  }

  /** Produces a sequence of `n` items by mutating the input `sel` selection.
    *
    * It assumes the invoking transaction is 'up-to-date' and will cause
    * the selection's cursors to step from this transaction's input access.
    */
  def mutate(sq: Vec[stm.Source[S#Tx, Chromosome]], n: Int,
             inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, Chromosome])] = {
    ???
//    var res = Vector.empty[(S#Acc, confluent.Source[S, Chromosome])]
//    while (res.size < n) {
//      val chosenH = sq(res.size % sq.size)
//      val csr     = global.forkCursor
//      val h0 = csr.stepFrom(inputAccess) { implicit tx =>
//        implicit val dtx = tx.durable
//        val chosen  = chosenH()
//        val sz      = chosen.size
//        // flip one or two bits
//        val two = rng.nextBoolean()
//        val i1  = rng.nextInt(sz)
//        chosen(i1).bit.transform(!_)
//        if (two) {
//          val i20 = rng.nextInt(sz - 1)
//          val i2  = if (i20 < i1) i20 else i20 + 1
//          chosen(i2).bit.transform(!_)
//        }
//        tx.newHandle(chosen)
//      }
//      val h   = h0 // csr.step { implicit tx => tx.newHandle(h0()) }
//      val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
//      if (DEBUG) println(s"$h - $pos")
//      res :+= (pos, h)
//    }
//    res
  }


  /** Performs one iteration of the algorithm, assuming that current population
    * was already evaluated. Steps:
    *
    * - elitism
    * - selection
    * - breeding: mutation and cross-over
    * - evaluation
    */
  def iterate(): Unit = {
    val (el, sel, pop, inputAccess) = global.cursor.step { implicit tx =>
      if (DEBUG) println(s"iterate - inputAccess ${tx.inputAccess}")
      val all   = genome.chromosomes()
      val _el   = elitism(all) // .toSet
    val _sel  = select (all) .map(tx.newHandle(_))
      (_el, scramble(_sel.toIndexedSeq), all.size, tx.inputAccess)
    }

    val nGen    = pop - el.size
    val nMut    = (mutationProb * nGen + 0.5).toInt
    val nCross  = nGen - nMut

    // if (DEBUG) println(s"pop $pop, el ${el.size}, sel ${sel.size}, nMut $nMut, nCross $nCross")

    val mut     = mutate    (sel, nMut  , inputAccess)
    val cross   = crossover (sel, nCross, inputAccess)

    global.cursor.step { implicit tx =>
      genome.chromosomes() = el ++ (mut ++ cross).map { case (access, h) =>
        h.meld(access)
      }
      evaluate()
    }
  }
}