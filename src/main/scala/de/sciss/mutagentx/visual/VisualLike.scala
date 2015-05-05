package de.sciss.mutagentx
package visual

import de.sciss.file._
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.swing.View
import de.sciss.processor.Processor
import prefuse.data.{Graph => PGraph}
import prefuse.visual.VisualGraph
import prefuse.{Visualization, Display}

trait VisualLike extends View[S] {
  def display: Display

  def visualization: Visualization

  def graph: PGraph

  def visualGraph: VisualGraph

  // def algorithm: Algorithm

  /** Schedule code to be executed during paused visualization animation
    * on the EDT after the commit of the transaction.
    */
  def deferVisTx(thunk: => Unit)(implicit tx: TxnLike): Unit

  def previousIteration(): Unit

  def animationStep(): Unit

  var runAnimation: Boolean

  def saveFrameAsPNG(file: File): Unit

  def saveFrameSeriesAsPNG(settings: VideoSettings): Processor[Unit]
}