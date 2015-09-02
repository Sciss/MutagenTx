/*
 *  SkipQuadtreeView.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.data.gui

import java.awt.{Color, Dimension}

import de.sciss.lucre.data.DeterministicSkipOctree
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.geom.{IntPoint2DLike, IntSpace}
import de.sciss.lucre.stm.{Cursor, Source, Sys}

class SkipQuadtreeView[S <: Sys[S], A](access: Source[S#Tx, DeterministicSkipOctree[S, TwoDim, A]],
                                       cursor: Cursor[S], pointView: A => IntPoint2DLike)
  extends QuadView {
  //   private type Child = txn.DeterministicSkipOctree.Node[ S, Space.IntTwoDim, A ]

  def t(implicit tx: S#Tx): DeterministicSkipOctree[S, IntSpace.TwoDim, A] = access()

  var highlight = Set.empty[A]
  var gridColor = new Color(0x00, 0x00, 0x00, 0x30)
  private var scaleVar = 1.0

  private val hyperCube = cursor.step { implicit tx => t.hyperCube }

  setPrefSz(3)

  def scale: Double = scaleVar

  def scale_=(factor: Double): Unit = scaleVar = factor

  private def setPrefSz(lvl: Int): Unit = {
    val w1 = ((hyperCube.extent.toLong << 1) * scale + 0.5).toInt + 1
    val in = getInsets
    setPreferredSize(new Dimension(((w1 + 16) * lvl - 16) + (in.left + in.right), w1 + (in.top + in.bottom)))
  }

  def adjustPreferredSize(): Unit =
    setPrefSz( cursor.step { implicit tx => t.numLevels })

  protected def draw(h: QuadView.PaintHelper): Unit = {
    var (tr, n) = cursor.step { implicit tx => val res = t; (res, res.headTree) }
    val q = hyperCube
    val dx = ((q.extent.toLong << 1) * scale + 0.5).toInt + 16
    h.scale= scale
    while (n != null) {
      draw(tr, h, n)
      h.translate(dx, 0)
      n = cursor.step { implicit tx => n.nextOption.orNull }
    }
  }

  private def draw(tr: DeterministicSkipOctree[S, IntSpace.TwoDim, A],
                   h: QuadView.PaintHelper, quad: DeterministicSkipOctree[S, IntSpace.TwoDim, A]#Child): Unit =
    quad match {
      case l: tr.Leaf =>
        h.drawPoint( pointView( l.value ), highlight.contains( l.value ))
      case n: tr.Branch =>
        for( idx <- 0 until 4 ) {
          h.drawFrame( n.hyperCube.orthant( idx ), gridColor )
          draw( tr, h, cursor.step { implicit tx => n.child( idx )})
        }
      case _ =>
    }
}