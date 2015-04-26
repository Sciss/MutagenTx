package de.sciss.mutagentx
package visual

import prefuse.data.{Edge => PEdge}

object VisualEdge {
  def apply(source: VisualNode, sink: VisualNode, init: Boolean)(implicit tx: S#Tx): VisualEdge.Init =
    impl.VisualEdgeImpl(source, sink, init = init)

  trait Init extends VisualEdge {
    def init()(implicit tx: S#Tx): this.type
  }
}
trait VisualEdge extends VisualData {
  def source: VisualNode
  def sink  : VisualNode

  def key: (VisualNode, VisualNode)
  def pEdge : PEdge
}