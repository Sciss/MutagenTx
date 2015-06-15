/*
 *  Genome.scala
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

import de.sciss.lucre.event.{InMemory, Sys}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Mutable, MutableSerializer}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object Genome {
  def empty[S <: Sys[S]](implicit tx: S#Tx): Genome[S] = {
    val id          = tx.newID()
    val chromosomes = ??? : S#Var[Vec[Chromosome[S]]] // tx.newVar(id, Vec.empty[Chromosome[S]])
    val fitness     = tx.newVar(id, Vec.empty[Float     ])(Serializer.indexedSeq)
    val cursor      = ??? : stm.Cursor[S] // tx.system.newCursor()
    new GenomeImpl[S](id, chromosomes, fitness, cursor)
  }

  private final class GenomeImpl[S <: Sys[S]](val id         : S#ID,
                                 val chromosomes: S#Var[Vec[Chromosome[S]]],
                                 val fitness    : S#Var[Vec[Float     ]],
                                 val cursor     : stm.Cursor[S] /* confluent.Cursor[S, D] */)
    extends Genome[S] with Mutable.Impl[S] {

    override def toString() = s"Genome$id"

    protected def writeData(out: DataOutput): Unit = {
      chromosomes .write(out)
      fitness     .write(out)
      ??? // cursor      .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      // implicit val dtx = tx.durable
      chromosomes .dispose()
      fitness     .dispose()
      ??? // cursor      .dispose()
    }
  }

  implicit def Ser[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Genome[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends MutableSerializer[S, Genome[S]] {
    protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): Genome[S] = {
      val chromosomes   = ??? : S#Var[Vec[Chromosome[S]]] // tx.readVar[Vec[Chromosome[S]]](id, in)
      val fitness       = tx.readVar[Vec[Float     ]](id, in)(Serializer.indexedSeq)
      val cursor        = ??? : stm.Cursor[S] // tx.system.readCursor(in)
      new GenomeImpl[S](id, chromosomes, fitness, cursor)
    }
  }
}
trait Genome[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  def chromosomes: S#Var[Vec[Chromosome[S]]]
  def fitness    : S#Var[Vec[Float     ]]

  // def cursor: confluent.Cursor[S, D]

  def cursor: stm.Cursor[S]
}