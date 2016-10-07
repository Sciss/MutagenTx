/*
 *  VisualVertex.scala
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

import de.sciss.lucre.stm.Sys
import de.sciss.synth.UGenSpec

object VisualUGen {
  def apply[S <: Sys[S]](main: Visual[S], v: Vertex.UGen[S])(implicit tx: S#Tx): VisualUGen[S] =
    impl.VisualUGenImpl(main, v)
}
trait VisualUGen[S <: Sys[S]] extends VisualVertex[S] {
  def info: UGenSpec
}

object VisualConstant {
  def apply[S <: Sys[S]](main: Visual[S], v: Vertex.Constant[S])(implicit tx: S#Tx): VisualConstant[S] =
    impl.VisualConstantImpl(main, v)
}
trait VisualConstant[S <: Sys[S]] extends VisualVertex[S] {
  var value: Float
}

object VisualVertex {
  def apply[S <: Sys[S]](main: Visual[S], v: Vertex[S])(implicit tx: S#Tx): VisualVertex[S] = v match {
    case vu: Vertex.UGen[S]      => VisualUGen    (main, vu)
    case vc: Vertex.Constant[S]  => VisualConstant(main, vc)
  }
}
sealed trait VisualVertex[S <: Sys[S]] extends VisualNode[S] {
  def name: String
}