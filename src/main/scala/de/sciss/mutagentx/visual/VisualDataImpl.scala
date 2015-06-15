package de.sciss.mutagentx
package visual

import de.sciss.lucre.event.Sys

import scala.concurrent.stm.Ref

trait VisualDataImpl[S <: Sys[S]] {
  private[this] val active = Ref(-1)

  final def isActive(implicit tx: S#Tx): Boolean =
    ??? // active.get(tx.peer) == tx.inputAccess.term.toInt

  final def touch()(implicit tx: S#Tx): Unit =
    ??? // active.set(tx.inputAccess.term.toInt)(tx.peer)

}
