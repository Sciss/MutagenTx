/*
 *  BoxRenderer.scala
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

import java.awt.geom.Rectangle2D
import java.awt.{Color, FontMetrics, Graphics2D, Shape}

import de.sciss.lucre.stm.Sys
import prefuse.render.{AbstractShapeRenderer, Renderer}
import prefuse.visual.VisualItem

object BoxRenderer {
  final val MinBoxWidth         = 24
  final val DefaultBoxHeight    = 20 // 18

  def defaultFontMetrics: FontMetrics = Renderer.DEFAULT_GRAPHICS.getFontMetrics(Visual.condensedFont) // (Style.font)

//  private final val colrSel     = Color.blue // Style.selectionColor
//  private final val strkColrOk  = ColorLib.getColor(192, 192, 192)
  // private final val strkColrEdit= colrSel
  // private final val strkColrErr = ColorLib.getColor(240,   0,   0)
  private final val fillColr    = Color.black // Style.boxColor
  // private final val textColrEdit= strkColrEdit
  private final val textColr    = Color.white // Color.black
//  private final val strkShpOk   = new BasicStroke(1f)
  // private final val strkShpPend = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f, Array[Float](6, 4), 0f)
//  private final val portColr    = Color.gray // Style.portColor
}
final class BoxRenderer[S <: Sys[S]](d: Visual[S]) extends AbstractShapeRenderer {
  import BoxRenderer._

  private val r   = new Rectangle2D.Float()
  // private val r2  = new Rectangle2D.Float()

  protected def getRawShape(vi: VisualItem): Shape = {
    var x    = vi.getX
    if (x.isNaN || x.isInfinity) x = 0.0
    var y    = vi.getY
    if (y.isNaN || y.isInfinity) y = 0.0

    vi.get(Visual.COL_MUTA) match {
      case data: VisualVertex[S] =>
        // UGenRenderer.getShape(x, y, data)
        data.getShape(x, y)
      case _ =>
        println("OOPS")
        r.setRect(x, y, MinBoxWidth, DefaultBoxHeight)
        r
    }
      // data.renderer.getShape(x, y, data)

    //    d.getNodeData(vi).fold[Shape] {
    //      r.setRect(x, y, MinBoxWidth, DefaultBoxHeight)
    //      r
    //    } { data =>
    //      data.renderer.getShape(x, y, data)
    //    }
  }

  override def render(g: Graphics2D, vi: VisualItem): Unit = {
    val shp = getShape(vi)
    // val b   = shp.getBounds2D
    g.setColor(fillColr)
    g.fill(shp)

    // g.setColor (strkColrOk)
    // g.setStroke(strkShpOk )
    // g.draw(shp)
    g.setColor(textColr)
    g.setFont(Visual.condensedFont) // Style.font
      // val fm  = Renderer.DEFAULT_GRAPHICS.getFontMetrics(Style.font)

    vi.get(Visual.COL_MUTA) match {
      case data: VisualVertex[S] =>
        data.render(g, vi)
        // UGenRenderer.paint(g, b, data)
        // data.renderer.paint(g, b, data)

      //      val ports   = data.ports
      //      if (ports.nonEmpty) {
      //        val atOrig  = g.getTransform
      //        val x       = b.getX.toFloat
      //        val y       = b.getY.toFloat
      //        g.translate(x, y)
      //        g.setColor(portColr)
      //        ports.inlets .foreach(g.fill)
      //        ports.outlets.foreach(g.fill)
      //        ports.active.foreach { p =>
      //          val r0 = p.visualRect(ports)
      //          g.setColor(colrSel)
      //          r.setRect(r0.getX - 1, r0.getY - 1, r0.getWidth + 2, r0.getHeight + 2)
      //          g.fill(r0)
      //        }
      //        g.setTransform(atOrig)
      //      }
    }
  }
}

trait ElementRenderer[S <: Sys[S]] {
  def getShape(x: Double, y: Double, data: VisualVertex[S]): Shape

  def paint(g: Graphics2D, bounds: Rectangle2D, data: VisualVertex[S]): Unit
}

trait StringRendererLike[S <: Sys[S]] extends ElementRenderer[S] {
  private val r = new Rectangle2D.Float()

  protected def dataToString(data: VisualVertex[S]): String

  def getShape(x: Double, y: Double, data: VisualVertex[S]): Shape = {
    val fm    = BoxRenderer.defaultFontMetrics
    val w1    = fm.stringWidth(dataToString(data))
    val w2    = math.max(BoxRenderer.MinBoxWidth, w1 + 6)
    // val ports = data.ports
    // val w3    = math.max(ports.numIns, ports.numOuts) * VisualPorts.MinSpacing
    val w     = w2 // math.max(w2, w3)
    r.setRect(x, y, w, BoxRenderer.DefaultBoxHeight)
    r
  }

  def paint(g: Graphics2D, bounds: Rectangle2D, data: VisualVertex[S]): Unit = {
    val x   = bounds.getX.toFloat
    val y   = bounds.getY.toFloat
    // g.setFont(Style.font)
    val fm  = g.getFontMetrics
    g.drawString(dataToString(data), x + 3, y + 2 + fm.getAscent)
  }
}

//object ToStringRenderer extends StringRendererLike {
//  protected def dataToString(data: VisualVertex) = data.value.toString
//}

//object BooleanRenderer extends ElementRenderer {
//  final val DefaultWidth  = 16
//  final val DefaultHeight = 16
//
//  private val ln  = new Line2D.Float()
//  private val r   = new Rectangle2D.Float()
//
//  def getShape(x: Double, y: Double, data: VisualVertex): Shape = {
//    r.setRect(x, y, DefaultWidth, DefaultHeight)
//    r
//  }
//
//  def paint(g: Graphics2D, bounds: Rectangle2D, data: VisualVertex): Unit =
//    data.value match {
//      case true =>
//        g.setColor(Color.black)
//        ln.setLine(bounds.getMinX + 2, bounds.getMinY + 2, bounds.getMaxX - 2, bounds.getMaxY - 2)
//        g.draw(ln)
//        ln.setLine(bounds.getMinX + 2, bounds.getMaxY - 2, bounds.getMaxX - 2, bounds.getMinY + 2)
//        g.draw(ln)
//      case _ =>
//    }
//}

final class UGenRenderer[S <: Sys[S]] extends StringRendererLike[S] {
  protected def dataToString(data: VisualVertex[S]) = data.name
}