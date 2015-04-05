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

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.store.BerkeleyDB

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object Algorithm {
  def apply(): Algorithm = {
    // val n = 20
    val dbf = BerkeleyDB.tmp()
    // system.rootWithDurable(...)
    new Algorithm {
      val system = ConfluentReactive(dbf)
      implicit val rngSer = TxnRandom.Persistent.serializer[D]
      val (handle, rng) = system.rootWithDurable { implicit tx =>
        implicit val dtx = system.durableTx(tx)
        val id = tx.newID()
        Genome.empty
      } { implicit tx =>
        TxnRandom.Persistent[D]()
      }

//      val (handle, cursor) = system.cursorRoot { implicit tx =>
//        implicit val dtx  = system.durableTx(tx)
//        implicit val r    = TxnRandom.Persistent[D]()
//        Genome(n)
//      } { implicit tx => _ => system.newCursor() }

      def genome(implicit tx: S#Tx): Genome = handle()
    }
  }
}
trait Algorithm {
  def genome(implicit tx: S#Tx): Genome

  def system: S

  implicit def rng: TxnRandom.Persistent[D]

  def init(n: Int)(implicit tx: S#Tx): Unit = {
    genome.chromosomes() = Vector.fill(n)(ChromosomeH(8))
  }

  def select()(implicit tx: S#Tx): Set[ChromosomeH] = {
    implicit val dtx = tx.durable

    val prev  = genome.chromosomes.apply()
    val frac  = 0.2
    val pop   = prev.size
    val n     = (pop * frac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[ChromosomeH], out: Set[ChromosomeH]): Set[ChromosomeH] = if (rem == 0) out else {
      val sum     = in.view.map(_.fitness).sum
      val rem1    = rem - 1
      if (sum == 0.0) {
        val chosen = in.head
        loop(rem1, in - chosen, out + chosen)
      } else {
        val inIdx       = in.zipWithIndex[ChromosomeH, Vec[(ChromosomeH, Int)]](breakOut)
        val norm        = inIdx.map {
          case (c, j) => (j, c.fitness / sum)
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

    val all   = prev.toSet
    val sel   = loop(n, all, Set.empty)
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  def elitism()(implicit tx: S#Tx): Vec[ChromosomeH] = {
    val n = 4
    val sel = genome.chromosomes.apply().sortBy(-_.fitness).take(n)
    sel
  }

  def mutate()(implicit tx: S#Tx): Unit = {

  }

  // def rng(implicit tx: S#Tx): TxnRandom[S#Tx]

  def iterate(): Unit = {
//    val el  = elitism().toSet
//    implicit val r = rng
//    val sel = select()


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
