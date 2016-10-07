/*
 *  VisualDataImpl.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx
package visual

import de.sciss.lucre.confluent
import de.sciss.lucre.stm.Sys

import scala.concurrent.stm.Ref

trait VisualDataImpl[S <: Sys[S]] {
  private[this] val active = Ref(-1)

  final def isActive(implicit tx: S#Tx): Boolean = tx match {
    case ctx: confluent.Txn[_] =>
      active.get(tx.peer) == ctx.inputAccess.term.toInt
    case _ => true
  }

  final def touch()(implicit tx: S#Tx): Unit = tx match {
    case ctx: confluent.Txn[_] =>
      active.set(ctx.inputAccess.term.toInt)(tx.peer)
    case _ =>
  }
}
