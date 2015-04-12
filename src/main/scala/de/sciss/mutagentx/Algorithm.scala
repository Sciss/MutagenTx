/*
 *  Algorithm
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

import de.sciss.lucre.{stm, confluent}
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.store.BerkeleyDB

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object Algorithm {
  val DEBUG = false

  def apply(): Algorithm = {
    val dbf = BerkeleyDB.tmp()
    new Algorithm {
      implicit val system = ConfluentReactive(dbf)
      val (handle, global) = system.rootWithDurable { implicit tx =>
        implicit val dtx = system.durableTx(tx)
        Genome.empty
      } { implicit tx =>
        GlobalState()
      }

      def genome(implicit tx: S#Tx): Genome = handle()
    }
  }
}
trait Algorithm {
  import Algorithm.DEBUG

  def genome(implicit tx: S#Tx): Genome

  def system: S

  val global: GlobalState

  import global.rng

  def init(n: Int)(implicit tx: S#Tx): Unit =
    genome.chromosomes() = Vector.fill(n)(ChromosomeH(8))

  def evaluate()(implicit tx: S#Tx): Unit =
    genome.chromosomes().foreach { cH =>
      val b = cH.peer.bits
      val h = b.size / 2
      // simple example function: lhs should be true, rhs should be false
      val c = b.zipWithIndex.count { case (v, i) => v() == (i < h) }
      cH.fitness() = c.toDouble / b.size
    }

  def print()(implicit tx: S#Tx): Unit = {
    val s = mkString()
    tx.afterCommit(println(s))
  }

  def mkString()(implicit tx: S#Tx): String =
    genome.chromosomes().zipWithIndex.map { case (cH, i) =>
      val b = cH.peer.bits.map { v => if (v()) '1' else '0' } .mkString
      f"${i + 1}%2d  ${cH.fitness()}%1.3f  $b"
    } .mkString("\n")

  def select(all: Vec[ChromosomeH])(implicit tx: S#Tx): Set[ChromosomeH] = {
    implicit val dtx = tx.durable

    val frac  = 0.2
    val pop   = all.size
    val n     = (pop * frac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[ChromosomeH], out: Set[ChromosomeH]): Set[ChromosomeH] = if (rem == 0) out else {
      val sum     = in.view.map(_.fitness()).sum
      val rem1    = rem - 1
      if (sum == 0.0) {
        val chosen = in.head
        loop(rem1, in - chosen, out + chosen)
      } else {
        val inIdx       = in.zipWithIndex[ChromosomeH, Vec[(ChromosomeH, Int)]](breakOut)
        val norm        = inIdx.map {
          case (c, j) => (j, c.fitness() / sum)
        }
        val sorted      = norm.sortBy(_._2)
        val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
        val roulette    = rng.nextDouble()
        val idxS        = acc.indexWhere(_ > roulette)
        val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
        val (chosen, _) = inIdx(idx)
        loop(rem1, in - chosen, out + chosen)
      }
    }

    val sel   = loop(n, all.toSet, Set.empty)
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  def elitism(all: Vec[ChromosomeH])(implicit tx: S#Tx): Vec[ChromosomeH] = {
    val n = 2
    val sel = all.sortBy(-_.fitness()).take(n)
    sel
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
  def crossover(sq: Vec[stm.Source[S#Tx, ChromosomeH]], n: Int,
                inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, ChromosomeH])] = {
    var res = Vector.empty[(S#Acc, confluent.Source[S, ChromosomeH])]
    while (res.size < n) {
      val idx0      = res.size << 1
      val chosen0H  = sq( idx0      % sq.size)
      val chosen1H  = sq((idx0 + 1) % sq.size)
      val csr       = global.forkCursor
      val h = csr.stepFrom(inputAccess) { implicit tx =>
        implicit val dtx = tx.durable
        val bits0   = chosen0H().peer.bits
        val bits1   = chosen1H().peer.bits
        val numBits = bits0.size
        require(numBits > 1)
        val split   = rng.nextInt(numBits - 1) + 1
        val bits    = bits0.take(split) ++ bits1.drop(split)
        val c       = Chromosome(bits)
        val cH      = ChromosomeH(c)
        tx.newHandle(cH)
      }
      val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
      if (DEBUG) println(s"$h - $pos")
      res :+= (pos, h)
    }
    res
  }

  /** Produces a sequence of `n` items by mutating the input `sel` selection.
    *
    * It assumes the invoking transaction is 'up-to-date' and will cause
    * the selection's cursors to step from this transaction's input access.
    */
  def mutate(sq: Vec[stm.Source[S#Tx, ChromosomeH]], n: Int,
             inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, ChromosomeH])] = {
    var res = Vector.empty[(S#Acc, confluent.Source[S, ChromosomeH])]
    while (res.size < n) {
      val chosenH = sq(res.size % sq.size)
      val csr     = global.forkCursor
      val h0 = csr.stepFrom(inputAccess) { implicit tx =>
        implicit val dtx = tx.durable
        val chosen = chosenH()
        val b   = chosen.peer.bits
        // flip one or two bits
        val two = rng.nextBoolean()
        val i1  = rng.nextInt(b.size)
        b(i1).transform(!_)
        if (two) {
          val i20 = rng.nextInt(b.size - 1)
          val i2  = if (i20 < i1) i20 else i20 + 1
          b(i2).transform(!_)
        }
        tx.newHandle(chosen)
      }
      val h = h0 // csr.step { implicit tx => tx.newHandle(h0()) }
      val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
      if (DEBUG) println(s"$h - $pos")
      res :+= (pos, h)
    }
    res
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
    val wMut    = 0.5
    val nMut    = (wMut * nGen + 0.5).toInt
    val nCross  = nGen - nMut
    val mut     = mutate    (sel, nMut  , inputAccess)
    val cross   = crossover (sel, nCross, inputAccess)

    global.cursor.step { implicit tx =>
      genome.chromosomes() = el ++ (mut ++ cross).map { case (access, h) =>
        h.meld(access)
      }
      evaluate()
    }

    /*


      [c1, c2, c3, c4]

      genome {
         evolution: LinkedList[D, Vector[ChromosomeH]]
      }

      ChromosomeH {
        cursor: confluent.Cursor[S]
        vr: S#Var[Chromosome]
      }


     */
  }
}
