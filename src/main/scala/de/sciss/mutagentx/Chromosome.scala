/*
 *  Chromosome
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

import scala.collection.immutable.{IndexedSeq => Vec}

object Chromosome {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): Chromosome = new Chromosome {
    val id    = tx.newID()
    val bits  = Vector.fill(numBits)(tx.newBooleanVar(id, r.nextBoolean()(tx.durable)))
  }

  def apply(b: Vec[S#Var[Boolean]])(implicit tx: S#Tx): Chromosome = new Chromosome {
    val id    = tx.newID()
    val bits  = b
  }

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Chromosome = Ser.read(in, access)

  implicit object Ser extends MutableSerializer[S, Chromosome] {
    def readData(in: DataInput, id0: S#ID)(implicit tx: S#Tx): Chromosome = new Chromosome {
      val id    = id0
      val bits  = Vector.fill(in.readInt())(tx.readVar[Boolean](id, in))
    }
  }
}
trait Chromosome extends stm.Mutable.Impl[S] {
  def bits: Vec[S#Var[Boolean]]

  //  def vertices: SkipList.Set[S, Vertex]
  //  def edges   : SkipList.Set[S, Edge  ]
  //  def fitness : S#Var[Double]

  override def toString(): String = s"Chromosome$id"

  protected final def writeData(out: DataOutput): Unit = {
    out.writeInt(bits.size)
    bits.foreach(_.write(out))
  }

  protected final def disposeData()(implicit tx: S#Tx): Unit =
    bits.foreach(_.dispose())
}