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

import de.sciss.lucre.stm.InMemoryLike.{ID, Txn}
import de.sciss.lucre.stm.{NoSys, Copy, Mutable, MutableSerializer, Sys}
import de.sciss.lucre.{data, stm}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.Ref

object Genome {
  def empty[S <: Sys[S]](implicit tx: S#Tx): Genome[S] = {
    implicit val chrSer = Chromosome.serializer[S]
    val id          = tx.newID()
    val chromosomes = tx.newVar(id, Vec.empty[Chromosome[S]])
    val fitness     = tx.newVar(id, Vec.empty[Float])(Serializer.indexedSeq)
    new GenomeImpl[S](id, chromosomes, fitness)
  }

  // type Var[S <: Sys[S], A] = stm.Sink[S#Tx, A] with stm.Source[S#Tx, A]

  def DurableHybrid(global: GlobalState.DurableHybrid, peer: Genome[stm.Durable])
                   (implicit tx: stm.Durable#Tx): Genome[stm.InMemory] =
    new Genome[stm.InMemory] {
      type S = stm.InMemory
      type D = stm.Durable
      
      // type ChromoAux[~ <: Sys[~]]
      private val _global = global

      import global.dtx
      
      private def copyChromo[In <: Sys[In], Out <: Sys[Out]](in: Chromosomes[In])
                                                            (implicit txIn: In#Tx, txOut: Out#Tx): Chromosomes[Out] = {
        val context = Copy[In, Out](txIn, txOut)
        try {
          in.map(context(_))
        } finally {
          context.finish()
        }
      }
      
      private val cRef: Ref[Chromosomes[S]] = Ref(copyChromo(peer.chromosomes())(tx, tx.inMemory))
      private val fRef: Ref[Vec[Float]]     = Ref(peer.fitness())
  
      val chromosomes: Var[Chromosomes[S]] = 
        new stm.Source[S#Tx, Chromosomes[S]] with stm.Sink[S#Tx, Chromosomes[S]] {
          def apply()(implicit tx: S#Tx): Chromosomes[S] = cRef.get(tx.peer)

          def update(v: Chromosomes[S])(implicit tx: S#Tx): Unit = {
            cRef.set(v)(tx.peer)
            peer.chromosomes.update(copyChromo(v))
          }
        }

      val fitness: Var[Vec[Float]] =
        new stm.Source[S#Tx, Vec[Float]] with stm.Sink[S#Tx, Vec[Float]] {
          def apply()(implicit tx: S#Tx): Vec[Float] = fRef.get(tx.peer)

          def update(v: Vec[Float])(implicit tx: S#Tx): Unit = {
            fRef.set(v)(tx.peer)
            ???
          }
        }
  
      def dispose()(implicit tx: Txn[S]): Unit = id.dispose()
  
      def write(out: DataOutput): Unit = throw new UnsupportedOperationException
  
      val id: ID[S] = tx.inMemory.newID()
    }

  private final class GenomeImpl[S <: Sys[S]](val id: S#ID,
                                 val chromosomes: S#Var[Chromosomes[S]],
                                 val fitness    : S#Var[Vec[Float     ]])
    extends Genome[S] with Mutable.Impl[S] {

    override def toString = s"Genome$id"

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

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Genome[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends MutableSerializer[S, Genome[S]] {

    protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): Genome[S] = {
      val chromosomes   = tx.readVar[Chromosomes[S]](id, in)
      val fitness       = tx.readVar[Vec[Float     ]](id, in)(Serializer.indexedSeq)
      new GenomeImpl[S](id, chromosomes, fitness)
    }
  }
}
trait Genome[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type Var[A] = stm.Sink[S#Tx, A] with stm.Source[S#Tx, A]

  def chromosomes: Var[Chromosomes[S]]
  def fitness    : Var[Vec[Float     ]]
}