package de.sciss.mutagentx
package visual
package impl

import de.sciss.lucre.event.Sys
import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualUGenImpl {
  import VisualNodeImpl.diam

  def apply[S <: Sys[S]](_main: Visual[S], v: Vertex.UGen[S])(implicit tx: S#Tx): VisualUGen[S] =
    new VisualUGen[S] with VisualVertexImpl[S] {
      val info = v.info
      val name = v.boxName

      val main  = _main

      val active = ??? : Ref[Int] // Ref(tx.inputAccess.term.toInt)

      override def toString = s"VisualUGen($name)@${hashCode.toHexString}"

      protected def boundsResized(): Unit = ()

      protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
        drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)

      init()
    }
}