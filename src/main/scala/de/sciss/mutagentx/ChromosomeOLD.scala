/*
 *  Chromosome.scala
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

object ChromosomeOLD {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): ChromosomeOLD = new ChromosomeOLD {
    val id      = tx.newID()
    val head    = tx.newVar(id, Bit(numBits))
    val fitness = tx.newVar(id, 0.0)
  }

  def apply(c: Bit)(implicit tx: S#Tx): ChromosomeOLD = new ChromosomeOLD {
    val id      = tx.newID()
    val head    = tx.newVar(id, Option(c))
    val fitness = tx.newVar(id, 0.0)
  }

  implicit object Ser extends MutableSerializer[S, ChromosomeOLD] {
    def readData(in: DataInput, id0: S#ID)(implicit tx: S#Tx): ChromosomeOLD = {
      implicit val dtx = tx.durable
      implicit val sys = tx.system
      new ChromosomeOLD {
        val id      = id0
        val head    = tx.readVar[Option[Bit]](id, in)
        val fitness = tx.readVar[Double](id, in)
      }
    }
  }
}
trait ChromosomeOLD extends stm.Mutable.Impl[S] {
  def head    : S#Var[Option[Bit]]
  def fitness : S#Var[Double]

  final def bits(implicit tx: S#Tx): Vec[Boolean] = head().fold(Vector.empty[Boolean])(_.to[Vector])

  final def debugString(implicit tx: S#Tx): String = {
    @tailrec def loop(nOpt: Option[Bit], res: Vec[S#ID]): Vec[S#ID] = nOpt match {
      case Some(b) => loop(b.next(), res :+ b.id)
      case _ => res
    }

    val ids = loop(head(), Vector.empty)
    ids.mkString(s"c$id(", ", ", ")")
  }

  final def size(implicit tx: S#Tx): Int = head().fold(0)(_.size)

  def exists(p: Bit => Boolean)(implicit tx: S#Tx): Boolean = {
    @tailrec def loop(opt: Option[Bit]): Boolean = opt match {
      case Some(b) => if (p(b)) true else loop(b.next())
      case None => false
    }
    loop(head())
  }

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