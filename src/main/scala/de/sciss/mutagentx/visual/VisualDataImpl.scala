package de.sciss.mutagentx
package visual

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys

import scala.concurrent.stm.Ref

trait VisualDataImpl[S <: Sys[S]] {
  private[this] val active = Ref(-1)

  final def isActive(implicit tx: S#Tx): Boolean = {
    val ctx = tx.asInstanceOf[ConfluentReactive.Txn]  // XXX TODO
    active.get(tx.peer) == ctx.inputAccess.term.toInt
  }

  final def touch()(implicit tx: S#Tx): Unit = {
    val ctx = tx.asInstanceOf[ConfluentReactive.Txn]  // XXX TODO
    active.set(ctx.inputAccess.term.toInt)(tx.peer)
  }
}
