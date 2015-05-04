package de.sciss.mutagentx
package visual
package impl

import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualConstantImpl {
  import VisualNodeImpl.diam

  def apply(_main: Visual, v: Vertex.Constant)(implicit tx: S#Tx): VisualConstant = new VisualConstant with VisualNodeImpl {
    var value = v.f()
    val main  = _main

    val active = Ref(tx.inputAccess.term.toInt)

    override def toString = s"VisualConstant($value)@${hashCode.toHexString}"

    protected def boundsResized(): Unit = ()

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
      drawLabel(g, vi, diam * vi.getSize.toFloat * 0.5f, name)

    def name = value.toString

    init()
  }
}