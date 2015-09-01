package de.sciss.mutagentx
package visual

import java.awt.Shape

import de.sciss.lucre.stm.{Sys, Disposable}
import prefuse.data.{Node => PNode}
import prefuse.visual.VisualItem

import scala.concurrent.stm.TMap
import scala.swing.Graphics2D

/** The common trait of all visible objects on the
  * Prefuse display.
  *
  * The next sub-type is `VisualNode` that is represented by a graph node.
  */
trait VisualData[S <: Sys[S]] extends Disposable[S#Tx] {
  def main: VisualLike[S]

  def isActive(implicit tx: S#Tx): Boolean

  def touch()(implicit tx: S#Tx): Unit
}

trait VisualNode[S <: Sys[S]] extends VisualData[S] {
  def pNode: PNode
  def edgesIn : TMap[(VisualNode[S], VisualNode[S]), VisualEdge[S]]
  def edgesOut: TMap[(VisualNode[S], VisualNode[S]), VisualEdge[S]]

  /** GUI property: whether the node is allowed to move around
    * as part of the dynamic layout (`false`) or not (`true`).
    */
  var fixed: Boolean

  /** Asks the receiver to paint its GUI representation. */
  def render(g: Graphics2D, vi: VisualItem): Unit

  /** Called from drag-control: updates the
    * current geometric shape of the corresponding visual item.
    */
  def update(shp: Shape): Unit

  def getShape(x: Double, y: Double): Shape
}