package de.sciss.mutagentx
package visual
package impl

import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualUGenImpl {
  import VisualNodeImpl.diam

  def apply(_main: Visual, v: Vertex.UGen)(implicit tx: S#Tx): VisualUGen = new VisualUGen with VisualVertexImpl {
    val info = v.info
    val name = v.boxName

    val main  = _main

    val active = Ref(tx.inputAccess.term.toInt)

    override def toString = s"VisualUGen($name)@${hashCode.toHexString}"

    protected def boundsResized(): Unit = ()

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
      drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)

    init()
  }
}