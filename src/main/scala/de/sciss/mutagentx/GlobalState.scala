/*
 *  GlobalState.scala
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
import de.sciss.lucre.stm.DurableLike.ID
import de.sciss.lucre.stm.{Cursor, MutableSerializer, Sink, Source, Sys}
import de.sciss.lucre.{confluent, stm}
import de.sciss.mutagentx.impl.TxnRandomBridge
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

import scala.concurrent.stm.{Ref, TxnLocal}

object GlobalState {
  object InMemory {
    type S = stm.InMemory

    def apply()(implicit tx: S#Tx): GlobalState[S] = new GlobalState[S] {
      val id                    = tx.newID()
      val rng: TxnRandom[S#Tx]  = TxnRandom(id)
      val cursor: stm.Cursor[S] = tx.system
      val numIterations         = tx.newIntVar(id, 0)
    }
  }
  type InMemory = GlobalState[stm.InMemory]

  object DurableHybrid {
    type S = stm.InMemory
    type D = stm.Durable

    def apply(peer: GlobalState.Durable)(implicit tx: D#Tx): DurableHybrid = new DurableHybrid {
      // private[this] val system = tx.system
      private[this] val dtxRef = TxnLocal[D#Tx]()

      implicit def dtx(implicit tx: S#Tx): D#Tx = dtxRef.get(tx.peer)

      implicit val rng: TxnRandom[S#Tx] = new TxnRandom[S#Tx] {
        def setSeed(seed: Long)(implicit tx: S#Tx): Unit = peer.rng.setSeed(seed)

        def nextBoolean()(implicit tx: S#Tx): Boolean  = peer.rng.nextBoolean()
        def nextDouble ()(implicit tx: S#Tx): Double   = peer.rng.nextDouble ()
        def nextLong   ()(implicit tx: S#Tx): Long     = peer.rng.nextLong   ()
        def nextFloat  ()(implicit tx: S#Tx): Float    = peer.rng.nextFloat  ()
        def nextInt    ()(implicit tx: S#Tx): Int      = peer.rng.nextInt    ()

        def nextInt(n: Int)(implicit tx: S#Tx): Int    = peer.rng.nextInt(n)
      }

      val cursor: Cursor[S] = new Cursor[S] {
        def step[A](fun: S#Tx => A): A =
          peer.cursor.step { dtx =>
            dtxRef.set(dtx)(dtx.peer)
            fun(dtx.inMemory)
          }

        def position(implicit tx: S#Tx) = ()
      }

      val numIterations: Sink[S#Tx, Int] with Source[S#Tx, Int] =
        new Sink[S#Tx, Int] with Source[S#Tx, Int] {
          private val ref = Ref(peer.numIterations())

          def update(v: Int)(implicit tx: S#Tx): Unit = {
            ref.update(v)(tx.peer)
            peer.numIterations() = v
          }

          def apply()(implicit tx: S#Tx): Int = ref.get(tx.peer)
        }
    }
  }
  trait DurableHybrid extends GlobalState[stm.InMemory] {
    type S = stm.InMemory
    type D = stm.Durable

    implicit def dtx(implicit tx: S#Tx): D#Tx
  }

  object Durable {
    type S = stm.Durable

    def apply()(implicit tx: S#Tx): Durable = new Impl {
      val id            = tx.newID()
      val rng           = TxnRandom.Persistent[S]
      val numIterations = tx.newIntVar(id, 0)

      val cursor: stm.Cursor[S] = tx.system
    }

    implicit def serializer: Serializer[S#Tx, S#Acc, Durable] = Ser

    private trait Impl extends Durable with stm.Mutable.Impl[S] {
      override def rng: TxnRandom.Persistent[S]

      protected def disposeData()(implicit tx: S#Tx /* Txn[S] */): Unit = {
        rng           .dispose()
        numIterations .dispose()
      }

      protected def writeData(out: DataOutput): Unit = {
        rng           .write(out)
        numIterations .write(out)
      }
    }

    private object Ser extends MutableSerializer[S, Durable] {
      protected def readData(in: DataInput, _id: ID[S])(implicit tx: S#Tx /* Txn[S] */): Durable = new Impl {
        val id            = _id
        val rng           = TxnRandom.Persistent.read[S](in, ())
        val numIterations = tx.readIntVar(id, in)

        val cursor: stm.Cursor[S] = tx.system
      }
    }
  }
  trait Durable extends GlobalState[stm.Durable] with stm.Mutable[stm.Durable#ID, stm.Durable#Tx] {
    type S = stm.Durable

    override def numIterations: S#Var[Int]
  }

  object Confluent {
    type S = confluent.Confluent
    type D = confluent.Confluent#D

    private trait Impl extends Confluent {
      protected def id: D#ID
      protected def rngD: TxnRandom.Persistent[D]
      protected def iterVar: D#Var[Int]

      lazy val rng = TxnRandomBridge[S, D](rngD)(_.durable)

      def cursor    : confluent.Cursor[S, D]
      def forkCursor: confluent.Cursor[S, D]

      def write(out: DataOutput): Unit = {
        id        .write(out)
        rngD      .write(out)
        cursor    .write(out)
        forkCursor.write(out)
        iterVar   .write(out)
      }

      object numIterations extends Sink[S#Tx, Int] with Source[S#Tx, Int] {
        def update(v: Int)(implicit tx: S#Tx): Unit = iterVar.update(v)(tx.durable)

        def apply()(implicit tx: S#Tx): Int = iterVar.apply()(tx.durable)
      }
    }

    def apply()(implicit tx: D#Tx, system: S): Confluent = {
      new Impl {
        val id          = tx.newID()
        val rngD        = TxnRandom.Persistent[D](8L) // XXX TODO -- seed frozen for testing
        val cursor      = confluent.Cursor[S, D]()
        val forkCursor  = confluent.Cursor[S, D]()
        val iterVar     = tx.newIntVar(id, 0)
      }
    }

    implicit def serializer(implicit system: S): Serializer[D#Tx, D#Acc, Confluent] = new Ser

    private final class Ser(implicit system: S) extends Serializer[D#Tx, D#Acc, Confluent] {
      def read(in: DataInput, access: D#Acc)(implicit tx: D#Tx): Confluent = {
        new Impl {
          val id          = tx.readID(in, ())
          val rngD        = TxnRandom.Persistent.read[D](in, access)
          val cursor      = confluent.Cursor.read[S, D](in)
          val forkCursor  = confluent.Cursor.read[S, D](in)
          val iterVar     = tx.readIntVar(id, in)
        }
      }

      def write(g: Confluent, out: DataOutput): Unit = g.write(out)
    }
  }
  trait Confluent extends GlobalState[confluent.Confluent] with Writable {
    type S = confluent.Confluent
    type D = confluent.Confluent#D

    override def cursor : confluent.Cursor[S, D]
    def forkCursor      : confluent.Cursor[S, D]
  }
}
trait GlobalState[S <: Sys[S]] {
  implicit def rng: TxnRandom[S#Tx]
  //  def cursor    : confluent.Cursor[S, D]
  //  def forkCursor: confluent.Cursor[S, D]

  def cursor: stm.Cursor[S]

  def numIterations: stm.Sink[S#Tx, Int] with stm.Source[S#Tx, Int] // S#Var[Int]
}