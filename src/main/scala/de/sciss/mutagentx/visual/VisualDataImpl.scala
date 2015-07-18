package de.sciss.mutagentx
package visual

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys

import scala.concurrent.stm.Ref

trait VisualDataImpl[S <: Sys[S]] {
  private[this] val active = Ref(-1)

  final def isActive(implicit tx: S#Tx): Boolean = tx match {
    case ctx: ConfluentReactive.Txn =>
      active.get(tx.peer) == ctx.inputAccess.term.toInt
    case _ => true
  }

  final def touch()(implicit tx: S#Tx): Unit = tx match {
    case ctx: ConfluentReactive.Txn =>
      active.set(ctx.inputAccess.term.toInt)(tx.peer)
    case _ =>
  }
}
