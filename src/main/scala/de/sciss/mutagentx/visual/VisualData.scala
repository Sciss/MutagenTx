package de.sciss.mutagentx
package visual

import java.awt.Shape

import prefuse.data.{Node => PNode}
import prefuse.visual.VisualItem

import scala.swing.Graphics2D

/** The common trait of all visible objects on the
  * Prefuse display.
  *
  * The next sub-type is `VisualNode` that is represented by a graph node.
  */
trait VisualData {
  // ---- methods to be called on the EDT ----

  /** GUI property: whether the node is allowed to move around
    * as part of the dynamic layout (`false`) or not (`true`).
    */
  var fixed: Boolean

  /** Called from drag-control: updates the
    * current geometric shape of the corresponding visual item.
    */
  def update(shp: Shape): Unit

  /** Asks the receiver to paint its GUI representation. */
  def render(g: Graphics2D, vi: VisualItem): Unit

  /** GUI property: name displayed. */
  def name: String
}

trait VisualNode extends VisualData {
  def pNode: PNode
}