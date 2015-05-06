package de.sciss.mutagentx
package visual

import java.awt.Shape
import java.awt.geom.Ellipse2D

import prefuse.render.AbstractShapeRenderer
import prefuse.visual.VisualItem

import scala.swing.Graphics2D

class NuagesShapeRenderer(size: Int)
  extends AbstractShapeRenderer {

  import VisualOLD._

  private val ellipse = new Ellipse2D.Float()

  protected def getRawShape(vi: VisualItem): Shape = {
    var x = vi.getX
    if (x.isNaN || x.isInfinity) x = 0.0
    var y = vi.getY
    if (y.isNaN || y.isInfinity) y = 0.0
    val diam = size * vi.getSize
    if (diam > 1) {
      x -= diam / 2
      y -= diam / 2
    }
    ellipse.setFrame(x, y, diam, diam)
    ellipse
  }

  override def render(g: Graphics2D, vi: VisualItem): Unit = {
    val data = vi.get(VisualOLD.COL_MUTA).asInstanceOf[VisualNode]
    if (data == null) return
    data.update(getShape(vi))
    data.render(g, vi)
  }
}