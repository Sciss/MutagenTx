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
import de.sciss.lucre.stm.Identifiable
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object Chromosome {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): Chromosome = new Chromosome {
    val id    = tx.newID()
    val bits  = Vector.fill(numBits)(tx.newBooleanVar(id, r.nextBoolean()(tx.durable)))
  }

  implicit object Ser extends Serializer[S#Tx, S#Acc, Chromosome] {
    def write(c: Chromosome, out: DataOutput): Unit = {
      c.id.write(out)
      out.writeInt(c.bits.size)
      c.bits.foreach(_.write(out))
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Chromosome = new Chromosome {
      val id    = tx.readID(in, access)
      val bits  = Vector.fill(in.readInt())(tx.readVar[Boolean](id, in))
    }
  }
}
trait Chromosome extends Identifiable[S#ID] {
  def bits: Vec[S#Var[Boolean]]

  //  def vertices: SkipList.Set[S, Vertex]
  //  def edges   : SkipList.Set[S, Edge  ]
  //  def fitness : S#Var[Double]
}