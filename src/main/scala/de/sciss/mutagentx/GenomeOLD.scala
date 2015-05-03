/*
 *  GenomeOLD.scala
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
import de.sciss.lucre.stm.{Mutable, MutableSerializer}
import de.sciss.serial.{DataInput, DataOutput}

object GenomeOLD {
  def empty(implicit tx: S#Tx): GenomeOLD = {
    val id          = tx.newID()
    val chromosomes = tx.newVar(id, Vec.empty[ChromosomeOLD])
    val cursor      = tx.system.newCursor()
    new GenomeImpl(id, chromosomes, cursor)
  }

  private final class GenomeImpl(val id: S#ID, val chromosomes: S#Var[Vec[ChromosomeOLD]],
                                 val cursor: confluent.Cursor[S, D])
    extends GenomeOLD with Mutable.Impl[S] {

    override def toString() = s"Genome$id"

    protected def writeData(out: DataOutput): Unit = {
      chromosomes .write(out)
      cursor      .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      implicit val dtx = tx.durable
      chromosomes .dispose()
      cursor      .dispose()
    }
  }

  implicit object Ser extends MutableSerializer[S, GenomeOLD] {
    protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): GenomeOLD = {
      val chromosomes   = tx.readVar[Vec[ChromosomeOLD]](id, in)
      val cursor        = tx.system.readCursor(in)
      new GenomeImpl(id, chromosomes, cursor)
    }
  }
}
trait GenomeOLD extends Mutable[S#ID, S#Tx] {
  def chromosomes: S#Var[Vec[ChromosomeOLD]]

  def cursor: confluent.Cursor[S, D]
}