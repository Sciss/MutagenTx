package de.sciss.mutagentx
package visual

import java.awt.Shape

import de.sciss.lucre.stm.Disposable
import prefuse.data.{Node => PNode, Edge => PEdge}
import prefuse.visual.VisualItem

import scala.concurrent.stm.TSet
import scala.swing.Graphics2D

/** The common trait of all visible objects on the
  * Prefuse display.
  *
  * The next sub-type is `VisualNode` that is represented by a graph node.
  */
trait VisualData extends Disposable[S#Tx] {
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

  //  /** GUI property: name displayed. */
  //  def name: String

  def main: Visual
}

trait VisualNode extends VisualData {
  def pNode: PNode
  def edgesIn : TSet[VisualEdge]
  def edgesOut: TSet[VisualEdge]
}

object VisualEdge {
  def apply(source: VisualNode, sink: VisualNode, init: Boolean)(implicit tx: S#Tx): VisualEdge.Init = {
    val res = Impl(source, sink)
    if (init) res.init()
    res
  }

  trait Init extends VisualEdge {
    def init()(implicit tx: S#Tx): this.type
  }

  private final case class Impl(source: VisualNode, sink: VisualNode) extends Init {
    private var _pEdge: PEdge = _

    override def productPrefix: String = "VisualEdge"

    private def main = source.main

    def pEdge: PEdge = {
      if (_pEdge == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
      _pEdge
    }

    def init()(implicit tx: S#Tx): this.type = {
      implicit val itx = tx.peer
      source.edgesOut.add(this)
      sink  .edgesIn .add(this)
      main.deferVisTx(mkPEdge())
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      source.edgesOut.remove(this)
      sink  .edgesIn .remove(this)
      main.deferVisTx(main.graph.removeEdge(_pEdge))
    }

    private def mkPEdge(): Unit = {
      if (_pEdge != null) throw new IllegalStateException(s"Component $this has already been initialized")
      _pEdge  = main.graph.addEdge(source.pNode, sink.pNode)
      val vis = main.visualization
      val vi  = vis.getVisualItem(Visual.GROUP_GRAPH, _pEdge)
      vi.set(Visual.COL_MUTA, this)
      //      val sz  = nodeSize
      //      if (sz != 1.0f) vi.set(VisualItem.SIZE, sz)
      //      parent.aggr.addItem(vi)
    }
  }
}
trait VisualEdge extends Disposable[S#Tx] {
  def source: VisualNode
  def sink  : VisualNode
  def pEdge : PEdge
}