/*
 *  VisualUGenImpl.scala
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
package impl

import de.sciss.lucre.confluent
import de.sciss.lucre.stm.Sys
import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualUGenImpl {

  def apply[S <: Sys[S]](_main: Visual[S], v: Vertex.UGen[S])(implicit tx: S#Tx): VisualUGen[S] =
    new VisualUGen[S] with VisualVertexImpl[S] {
      val info = v.info
      val name = v.boxName

      val main  = _main

      val active = Ref[Int]({
        tx match {
          case ctx: confluent.Txn[_] =>
            ctx.inputAccess.term.toInt
          case _ => 0
        }
      })

      override def toString = s"VisualUGen($name)@${hashCode.toHexString}"

      protected def boundsResized(): Unit = ()

      protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
        drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)

      init()
    }
}