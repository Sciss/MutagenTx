/*
 *  Bit
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
import de.sciss.serial.{Serializer, DataInput, DataOutput}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object Bit {
  implicit object Ser extends MutableSerializer[S, Bit] {
    def readData(in: DataInput, id0: S#ID)(implicit tx: S#Tx): Bit = new Bit {
      val id    = id0
      val bit   = tx.readVar[Boolean](id, in)
      val next  = tx.readVar[Option[Bit]](id, in)
    }
  }

  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): Option[Bit] = {
    @tailrec def loop(nOpt: Option[Bit], n: Int): Option[Bit] =
      if (n <= 0) nOpt else {
        val c = new Bit {
          val id    = tx.newID()
          val bit   = tx.newBooleanVar(id, r.nextBoolean()(tx.durable))
          val next  = tx.newVar(id, nOpt) // (Serializer.option[S#Tx, S#Acc, Bit])
        }
        loop(Some(c), n - 1)
      }

    loop(None, numBits)
  }

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Bit = Ser.read(in, access)
}
trait Bit extends stm.Mutable.Impl[S] {
  def bit: S#Var[Boolean]
  def next: S#Var[Option[Bit]]

  def to[Col[_]](implicit tx: S#Tx, cbf: CanBuildFrom[Nothing, Boolean, Col[Boolean]]): Col[Boolean] = {
    val b = cbf()

    @tailrec def loop(nOpt: S#Var[Option[Bit]]): Unit = nOpt() match {
      case Some(n) =>
        b += n.bit()
        loop(n.next)
      case _ =>
    }

    loop(next)
    b.result()
  }

  def size(implicit tx: S#Tx): Int = {
    @tailrec def loop(nOpt: S#Var[Option[Bit]], res: Int): Int = nOpt() match {
      case Some(n) => loop(n.next, res + 1)
      case _ => res
    }

    loop(next, 1)
  }

  //  def vertices: SkipList.Set[S, Vertex]
  //  def edges   : SkipList.Set[S, Edge  ]
  //  def fitness : S#Var[Double]

  override def toString(): String = s"Bit$id" // (bit = $bit, next = $next)"

  protected final def writeData(out: DataOutput): Unit = {
    bit .write(out)
    next.write(out)
  }

  protected final def disposeData()(implicit tx: S#Tx): Unit = {
    bit .dispose()
    next.dispose()
  }
}