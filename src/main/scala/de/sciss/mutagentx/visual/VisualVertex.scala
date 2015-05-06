package de.sciss.mutagentx
package visual

import de.sciss.synth.UGenSpec

object VisualUGen {
  def apply(main: Visual, v: Vertex.UGen)(implicit tx: S#Tx): VisualUGen = impl.VisualUGenImpl(main, v)
}
trait VisualUGen extends VisualVertex {
  def info: UGenSpec
}

object VisualConstant {
  def apply(main: Visual, v: Vertex.Constant)(implicit tx: S#Tx): VisualConstant = impl.VisualConstantImpl(main, v)
}
trait VisualConstant extends VisualVertex {
  var value: Float
}

object VisualVertex {
  def apply(main: Visual, v: Vertex)(implicit tx: S#Tx): VisualVertex = v match {
    case vu: Vertex.UGen      => VisualUGen    (main, vu)
    case vc: Vertex.Constant  => VisualConstant(main, vc)
  }
}
sealed trait VisualVertex extends VisualNode {
  def name: String
}