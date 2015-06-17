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
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{confluent, event => evt, stm}
import de.sciss.mutagentx.impl.TxnRandomBridge
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

object GlobalState {
  object InMemory {
    type S = evt.InMemory

    def apply()(implicit tx: S#Tx): GlobalState[S] = new GlobalState[S] {
      val rng: TxnRandom[S#Tx] = TxnRandom(tx.newID())
      val cursor: stm.Cursor[S] = tx.system
    }
  }

  object Confluent {
    type S = ConfluentReactive
    type D = ConfluentReactive#D

    private trait Impl extends Confluent {
      protected def rngD: TxnRandom.Persistent[D]

      lazy val rng = TxnRandomBridge[S, D](rngD)(_.durable)

      def cursor    : confluent.Cursor[S, D]
      def forkCursor: confluent.Cursor[S, D]

      def write(out: DataOutput): Unit = {
        rngD      .write(out)
        cursor    .write(out)
        forkCursor.write(out)
      }
    }

    def apply()(implicit tx: D#Tx, system: S): Confluent = {
      new Impl {
        val rngD        = TxnRandom.Persistent[D](8L) // XXX TODO -- seed frozen for testing
        val cursor      = confluent.Cursor[S, D]()
        val forkCursor  = confluent.Cursor[S, D]()
      }
    }

    implicit def serializer(implicit system: S): Serializer[D#Tx, D#Acc, Confluent] = new Ser

    private final class Ser(implicit system: S) extends Serializer[D#Tx, D#Acc, Confluent] {
      def read(in: DataInput, access: D#Acc)(implicit tx: D#Tx): Confluent = {
        new Impl {
          val rngD        = TxnRandom.Persistent.read[D](in, access)
          val cursor      = confluent.Cursor.read[S, D](in)
          val forkCursor  = confluent.Cursor.read[S, D](in)
        }
      }

      def write(g: Confluent, out: DataOutput): Unit = g.write(out)
    }
  }
  trait Confluent extends GlobalState[ConfluentReactive] with Writable {
    type S = ConfluentReactive
    type D = ConfluentReactive#D

    override def cursor : confluent.Cursor[S, D]
    def forkCursor      : confluent.Cursor[S, D]
  }
}
trait GlobalState[S <: Sys[S]] {
  implicit def rng: TxnRandom[S#Tx]
  //  def cursor    : confluent.Cursor[S, D]
  //  def forkCursor: confluent.Cursor[S, D]

  def cursor: stm.Cursor[S]
}