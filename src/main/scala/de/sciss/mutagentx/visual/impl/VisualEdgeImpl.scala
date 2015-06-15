/*
 *  VisualEdgeImpl.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.event.Sys
import prefuse.data.{Edge => PEdge}

import scala.concurrent.stm.Ref

object VisualEdgeImpl {
  def apply[S <: Sys[S]](source: VisualNode[S], sink: VisualNode[S], init: Boolean)
                        (implicit tx: S#Tx): VisualEdge.Init[S] = {
    val res = Impl(source, sink)
    if (init) res.init()
    res
  }

  private final case class Impl[S <: Sys[S]](source: VisualNode[S], sink: VisualNode[S])
    extends VisualEdge.Init[S] with VisualDataImpl[S] {

    private[this] var _pEdge: PEdge = _

    override def productPrefix: String = "VisualEdge"

    def main = source.main

    def pEdge: PEdge = {
      if (_pEdge == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
      _pEdge
    }

    def key = (source, sink)

    private[this] val _init = Ref(initialValue = false)

    def init()(implicit tx: S#Tx): this.type = {
      implicit val itx = tx.peer
      require(!_init.swap(true), s"Already initialized: $this")

      source.edgesOut.put(key, this)
      sink  .edgesIn .put(key, this)
      touch()
      main.deferVisTx {
        if (Visual.DEBUG) println(s"MAKE EDGE $this")
        mkPEdge()
      }
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      source.edgesOut.remove(key)
      sink  .edgesIn .remove(key)
      main.deferVisTx {
        val g = main.graph
        if (pEdge.isValid) g.removeEdge(pEdge)
      }
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