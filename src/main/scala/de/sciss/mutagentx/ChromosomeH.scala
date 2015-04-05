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

object ChromosomeH {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): ChromosomeH = new ChromosomeH {
    val id      = tx.newID()
    val apply   = tx.newVar(id, Chromosome(numBits))
    val fitness = tx.newVar(id, 0.0)
  }

  implicit object Ser extends MutableSerializer[S, ChromosomeH] {
    def readData(in: DataInput, id0: S#ID)(implicit tx: S#Tx): ChromosomeH = {
      implicit val dtx = tx.durable
      implicit val sys = tx.system
      new ChromosomeH {
        val id      = id0
        val apply   = tx.readVar[Chromosome](id, in)
        val fitness = tx.readVar[Double    ](id, in)
      }
    }
  }
}
trait ChromosomeH extends stm.Mutable.Impl[S] {
  def apply   : S#Var[Chromosome]
  def fitness : S#Var[Double    ]

  override def toString(): String = s"ChromosomeH$id"

  protected final def writeData(out: DataOutput): Unit = {
    apply   .write(out)
    fitness .write(out)
  }

  protected final def disposeData()(implicit tx: S#Tx): Unit = {
    apply  .dispose()
    fitness.dispose()
  }
}