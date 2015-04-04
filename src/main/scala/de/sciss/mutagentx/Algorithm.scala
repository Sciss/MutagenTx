package de.sciss.mutagentx

import de.sciss.lucre.confluent.TxnRandom

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

trait Algorithm {
  def genome(implicit tx: S#Tx): Genome

  def select()(implicit tx: S#Tx, r: TxnRandom[S#Tx]): Set[Chromosome] = {
    val prev  = genome.chromosomes
    val frac  = 0.2
    val pop   = prev.size
    val n     = (pop * frac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[Chromosome], out: Set[Chromosome]): Set[Chromosome] = if (rem == 0) out else {
      val sum     = in.view.map(_.fitness()).sum
      val rem1    = rem - 1
      if (sum == 0.0) {
        val chosen = in.head
        loop(rem1, in - chosen, out + chosen)
      } else {
        val inIdx       = in.zipWithIndex[Chromosome, Vec[(Chromosome, Int)]](breakOut)
        val norm        = inIdx.map {
          case (c, j) => (j, c.fitness() / sum)
        }
        val sorted      = norm.sortBy(_._2)
        val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
        val roulette    = r.nextDouble()
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

  def elitism()(implicit tx: S#Tx): Vec[Chromosome] = {
    val n = 4
    val sel = genome.chromosomes.toIndexedSeq.sortBy(-_.fitness()).take(n)
    sel
  }

  def mutate()(implicit tx: S#Tx, r: TxnRandom[S#Tx]): Unit = {

  }

  def rng(implicit tx: S#Tx): TxnRandom[S#Tx]

  def iterate()(implicit tx: S#Tx): Unit = {
    val el  = elitism().toSet
    implicit val r = rng
    val sel = select()


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
