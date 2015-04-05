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

import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm.Identifiable
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object ChromosomeH {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): ChromosomeH = new ChromosomeH {
    val id      = tx.newID()
    val cursor  = tx.system.newCursor()
    val apply   = tx.newVar(id, Chromosome(numBits))
    val fitness = tx.newVar(id, 0.0)
  }

  implicit object Ser extends Serializer[S#Tx, S#Acc, ChromosomeH] {
    def write(c: ChromosomeH, out: DataOutput): Unit = {
      c.id      .write(out)
      c.cursor  .write(out)
      c.apply   .write(out)
      c.fitness .write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ChromosomeH = {
      implicit val dtx = tx.durable
      implicit val sys = tx.system
      new ChromosomeH {
        val id      = tx.readID(in, access)
        val cursor  = confluent.Cursor.read[S, D](in)
        val apply   = tx.readVar[Chromosome](id, in)
        val fitness = tx.readVar[Double    ](id, in)
      }
    }
  }
}
trait ChromosomeH extends Identifiable[S#ID] {
  def cursor  : confluent.Cursor[S, D]

  def apply   : S#Var[Chromosome]
  def fitness : S#Var[Double    ]
}