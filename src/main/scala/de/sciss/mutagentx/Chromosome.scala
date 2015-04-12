/*
 *  ChromosomeH
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.MutableSerializer
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

object Chromosome {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): Chromosome = new Chromosome {
    val id      = tx.newID()
    val head    = tx.newVar(id, Bit(numBits))
    val fitness = tx.newVar(id, 0.0)
  }

  def apply(c: Bit)(implicit tx: S#Tx): Chromosome = new Chromosome {
    val id      = tx.newID()
    val head    = tx.newVar(id, Option(c))
    val fitness = tx.newVar(id, 0.0)
  }

  implicit object Ser extends MutableSerializer[S, Chromosome] {
    def readData(in: DataInput, id0: S#ID)(implicit tx: S#Tx): Chromosome = {
      implicit val dtx = tx.durable
      implicit val sys = tx.system
      new Chromosome {
        val id      = id0
        val head    = tx.readVar[Option[Bit]](id, in) // Bit.read(in, id0.path)
        val fitness = tx.readVar[Double](id, in)
      }
    }
  }
}
trait Chromosome extends stm.Mutable.Impl[S] {
  def head    : S#Var[Option[Bit]]
  def fitness : S#Var[Double]

  final def bits(implicit tx: S#Tx): Vec[Boolean] = head().fold(Vector.empty[Boolean])(_.to[Vector])

  final def size(implicit tx: S#Tx): Int = head().fold(0)(_.size)

  def apply(idx: Int)(implicit tx: S#Tx): Bit = {
    require (idx >= 0)
    @tailrec def loop(rem: Int, nOpt: Option[Bit]): Bit = {
      val n = nOpt.getOrElse(throw new IndexOutOfBoundsException(idx.toString))
      if (rem == 0) n else loop(rem - 1, n.next())
    }

    loop(idx, head())
  }

  override def toString(): String = s"Chromosome$id"

  protected final def writeData(out: DataOutput): Unit = {
    head    .write(out)
    fitness .write(out)
  }

  protected final def disposeData()(implicit tx: S#Tx): Unit = {
    head   .dispose()
    fitness.dispose()
  }
}