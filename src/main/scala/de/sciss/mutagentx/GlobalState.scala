/*
 *  GlobalState
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
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object GlobalState {
  def apply()(implicit tx: D#Tx, system: ConfluentReactive): GlobalState = new GlobalState {
    val rng         = TxnRandom.Persistent[D](0L) // XXX TODO -- seed frozen for testing
    val cursor      = confluent.Cursor[S, D]()
    val forkCursor  = confluent.Cursor[S, D]()
  }

  implicit def serializer(implicit system: ConfluentReactive): Serializer[D#Tx, D#Acc, GlobalState] = new Ser

  private final class Ser(implicit system: ConfluentReactive) extends Serializer[D#Tx, D#Acc, GlobalState] {
    def read(in: DataInput, access: D#Acc)(implicit tx: D#Tx): GlobalState = new GlobalState {
      val rng         = TxnRandom.Persistent.read[D](in, access)
      val cursor      = confluent.Cursor.read[S, D](in)
      val forkCursor  = confluent.Cursor.read[S, D](in)
    }

    def write(g: GlobalState, out: DataOutput): Unit = {
      g.rng       .write(out)
      g.cursor    .write(out)
      g.forkCursor.write(out)
    }
  }
}
trait GlobalState {
  implicit def rng: TxnRandom.Persistent[D]
  def cursor    : confluent.Cursor[S, D]
  def forkCursor: confluent.Cursor[S, D]
}
