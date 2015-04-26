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

object VisualEdge {
  def apply(source: VisualNode, sink: VisualNode, init: Boolean)(implicit tx: S#Tx): VisualEdge.Init = {
    val res = Impl(source, sink)
    if (init) res.init()
    res
  }

  trait Init extends VisualEdge {
    def init()(implicit tx: S#Tx): this.type
  }

  private final case class Impl(source: VisualNode, sink: VisualNode) extends Init with VisualDataImpl {
    private var _pEdge: PEdge = _

    override def productPrefix: String = "VisualEdge"

    def main = source.main

    def pEdge: PEdge = {
      if (_pEdge == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
      _pEdge
    }

    def key = (source, sink)

    def init()(implicit tx: S#Tx): this.type = {
      implicit val itx = tx.peer
      source.edgesOut.put(key, this)
      sink  .edgesIn .put(key, this)
      touch()
      main.deferVisTx(mkPEdge())
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      source.edgesOut.remove(key)
      sink  .edgesIn .remove(key)
      main.deferVisTx(main.graph.removeEdge(pEdge))
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
trait VisualEdge extends VisualData {
  def source: VisualNode
  def sink  : VisualNode

  def key: (VisualNode, VisualNode)
  def pEdge : PEdge
}