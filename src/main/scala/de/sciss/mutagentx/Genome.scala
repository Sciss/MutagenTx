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

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.InMemoryLike.{ID, Txn}
import de.sciss.lucre.stm.{Copy, Mutable, MutableSerializer, NoSys, Sys}
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

  private def readHybrid(config: Algorithm.Config, global: GlobalState.DurableHybrid)
                        (implicit tx: stm.Durable#Tx): (Chromosomes[stm.InMemory], Vec[Float]) = {
    implicit val itx = tx.inMemory
    // val iter    = global.numIterations()
    val dir     = config.databaseFile
    val base    = dir.parent
    val prefix  = s"${dir.name}_iter"
    val suffix  = ".bin"
    val all     = base.children(f => f.name.startsWith(prefix) && f.name.endsWith(suffix)).map { f =>
      val n = f.name
      n.substring(prefix.length, n.length - suffix.length).toInt
    }
    val iter    = if (all.isEmpty) global.numIterations() else {
      val res = all.max
      global.initIterations(res + 1)
      res
    }

    val f0    = base / s"$prefix$iter$suffix"
    val f1    = base / s"$prefix${iter - 1}$suffix"
    if (!f1.isFile && !f0.isFile) return (Vector.empty, Vector.empty)
    val f     = if (f1.isFile) f1 else f0

    type I = stm.InMemory

    // not really cool to do this within the transaction,
    // but well, in the worst case we write the same
    // file more than once.
    val in = DataInput.open(f)
    try {
      var cb = Vector.newBuilder[Chromosome[I]]
      val fb = Vector.newBuilder[Float]
      while (in.position < in.size) {
        val input = SOMGenerator.Input.serializer.read(in)
        val g     = input.graph
        val fit   = input.fitness
        val c     = impl.ChromosomeImpl.mkChromosome[I](g)
        cb += c
        fb += fit
      }
      (cb.result(), fb.result())
    } finally {
      in.close()
    }
  }

  def DurableHybrid(config: Algorithm.Config, global: GlobalState.DurableHybrid, peer: Genome[stm.Durable])
                   (implicit tx: stm.Durable#Tx): Genome[stm.InMemory] = {
    val (c0, f0) = readHybrid(config, global)
    val genome = new Genome[stm.InMemory] {
      type S = stm.InMemory
      type D = stm.Durable

      private[this] val _global = global
      // import _global.dtx
      
//      private def copyChromosome[In <: Sys[In], Out <: Sys[Out]](in: Chromosomes[In])
//                                                                (implicit txIn: In#Tx, txOut: Out#Tx): Chromosomes[Out] = {
//        val context = Copy[In, Out](txIn, txOut)
//        try {
//          in.map(context(_))
//        } finally {
//          context.finish()
//        }
//      }
      
      private val cRef: Ref[Chromosomes[S]] = Ref(c0)
      private val fRef: Ref[Vec[Float]]     = Ref(f0)

      private[this] val updCIter = Ref(-1)
      private[this] val updFIter = Ref(-1)

      // called as soon as both cx and fx have been set within the same iteration
      private[this] def flush(iter: Int, cx: Chromosomes[S], fx: Vec[Float])(implicit tx: S#Tx): Unit = {
        val dir   = config.databaseFile
        val f     = dir.parent / s"${dir.name}_iter$iter.bin"
        // not really cool to do this within the transaction,
        // but well, in the worst case we write the same
        // file more than once.
        val out   = DataOutput.open(f)
        try {
          (cx zip fx).foreach { case (c, fit) =>
            val graph = MkSynthGraph(c, mono = true, removeNaNs = false, config = true, ranges = false)
            val input = SOMGenerator.Input(graph, iter = iter, fitness = fit)
            SOMGenerator.Input.serializer.write(input, out)
          }
        } finally {
          out.close()
        }
      }

      val chromosomes: Var[Chromosomes[S]] = 
        new stm.Source[S#Tx, Chromosomes[S]] with stm.Sink[S#Tx, Chromosomes[S]] {
          def apply()(implicit tx: S#Tx): Chromosomes[S] = cRef.get(tx.peer)

          def update(cx: Chromosomes[S])(implicit tx: S#Tx): Unit = {
            implicit val itx = tx.peer
            cRef()      = cx
            val fIter   = updFIter()
            val iter    = _global.numIterations()
            updCIter()  = iter
            if (fIter == iter) flush(iter, cx, fitness())
          }
        }

      val fitness: Var[Vec[Float]] =
        new stm.Source[S#Tx, Vec[Float]] with stm.Sink[S#Tx, Vec[Float]] {
          def apply()(implicit tx: S#Tx): Vec[Float] = fRef.get(tx.peer)

          def update(fx: Vec[Float])(implicit tx: S#Tx): Unit = {
            implicit val itx = tx.peer
            fRef()      = fx
            val cIter   = updCIter()
            val iter    = _global.numIterations()
            updFIter()  = iter
            if (cIter == iter) flush(iter, chromosomes(), fx)
          }
        }
  
      def dispose()(implicit tx: Txn[S]): Unit = id.dispose()
  
      def write(out: DataOutput): Unit = throw new UnsupportedOperationException
  
      val id: ID[S] = tx.inMemory.newID()
    }
    genome
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