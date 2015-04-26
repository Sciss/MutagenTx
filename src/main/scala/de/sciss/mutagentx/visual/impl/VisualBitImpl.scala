/*
 *  VisualBitImpl.scala
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
package visual
package impl

import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualBitImpl {
  import VisualNodeImpl.diam

  def apply(_main: Visual, b: Bit)(implicit tx: S#Tx): VisualBit = new VisualBit with VisualNodeImpl {
    var state = b.bit()
    val main  = _main

    val active = Ref(tx.inputAccess.term.toInt)

    override def toString = s"VisualBit($state)@${hashCode.toHexString}"

    protected def boundsResized(): Unit = ()

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
      drawLabel(g, vi, diam * vi.getSize.toFloat * 0.5f, name)

    def name = if (state) "1" else "0"

    init()
  }
}