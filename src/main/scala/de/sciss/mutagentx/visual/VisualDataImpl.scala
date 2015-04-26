package de.sciss.mutagentx
package visual

import scala.concurrent.stm.Ref

trait VisualDataImpl {
  private[this] val active = Ref(-1)

  final def isActive(implicit tx: S#Tx): Boolean =
    active.get(tx.peer) == tx.inputAccess.term.toInt

  final def touch()(implicit tx: S#Tx): Unit =
    active.set(tx.inputAccess.term.toInt)(tx.peer)

}
