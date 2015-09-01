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

import de.sciss.lucre.data
import de.sciss.lucre.stm.{Sys, Mutable, MutableSerializer}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object Genome {
  def empty[S <: Sys[S]](implicit tx: S#Tx, ord: data.Ordering[S#Tx, Vertex[S]]): Genome[S] = {
    implicit val chrSer = Chromosome.serializer[S, Vertex, Edge]
    val id          = tx.newID()
    val chromosomes = tx.newVar(id, Vec.empty[Chromosome[S]])
    val fitness     = tx.newVar(id, Vec.empty[Float])(Serializer.indexedSeq)
    new GenomeImpl[S](id, chromosomes, fitness)
  }

  private final class GenomeImpl[S <: Sys[S]](val id: S#ID,
                                 val chromosomes: S#Var[Vec[Chromosome[S]]],
                                 val fitness    : S#Var[Vec[Float     ]])
    extends Genome[S] with Mutable.Impl[S] {

    override def toString() = s"Genome$id"

    protected def writeData(out: DataOutput): Unit = {
      chromosomes .write(out)
      fitness     .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      // implicit val dtx = tx.durable
      chromosomes .dispose()
      fitness     .dispose()
    }
  }

  implicit def Ser[S <: Sys[S]](implicit ord: data.Ordering[S#Tx, Vertex[S]]): Serializer[S#Tx, S#Acc, Genome[S]] =
    new Ser[S]

  private final class Ser[S <: Sys[S]](implicit ord: data.Ordering[S#Tx, Vertex[S]])
    extends MutableSerializer[S, Genome[S]] {

    protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): Genome[S] = {
      val chromosomes   = tx.readVar[Vec[Chromosome[S]]](id, in)
      val fitness       = tx.readVar[Vec[Float     ]](id, in)(Serializer.indexedSeq)
      new GenomeImpl[S](id, chromosomes, fitness)
    }
  }
}
trait Genome[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  def chromosomes: S#Var[Vec[Chromosome[S]]]
  def fitness    : S#Var[Vec[Float     ]]
}