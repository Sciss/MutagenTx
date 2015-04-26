package de.sciss.mutagentx
package visual

import java.awt.Shape

import de.sciss.lucre.stm.Disposable
import prefuse.data.{Node => PNode, Edge => PEdge}
import prefuse.visual.VisualItem

import scala.concurrent.stm.{TMap, Ref, TSet}
import scala.swing.Graphics2D

/** The common trait of all visible objects on the
  * Prefuse display.
  *
  * The next sub-type is `VisualNode` that is represented by a graph node.
  */
trait VisualData extends Disposable[S#Tx] {
  def main: Visual

  def isActive(implicit tx: S#Tx): Boolean

  def touch()(implicit tx: S#Tx): Unit
}

trait VisualNode extends VisualData {
  def pNode: PNode
  def edgesIn : TMap[(VisualNode, VisualNode), VisualEdge]
  def edgesOut: TMap[(VisualNode, VisualNode), VisualEdge]

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
}