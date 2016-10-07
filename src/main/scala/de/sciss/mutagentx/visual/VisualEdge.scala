/*
 *  VisualEdge.scala
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
import prefuse.data.{Edge => PEdge}

object VisualEdge {
  def apply[S <: Sys[S]](source: VisualNode[S], sink: VisualNode[S], init: Boolean)
                        (implicit tx: S#Tx): VisualEdge.Init[S] =
    impl.VisualEdgeImpl(source, sink, init = init)

  trait Init[S <: Sys[S]] extends VisualEdge[S] {
    def init()(implicit tx: S#Tx): this.type
  }
}
trait VisualEdge[S <: Sys[S]] extends VisualData[S] {
  def source: VisualNode[S]
  def sink  : VisualNode[S]

  def key: (VisualNode[S], VisualNode[S])
  def pEdge : PEdge
}