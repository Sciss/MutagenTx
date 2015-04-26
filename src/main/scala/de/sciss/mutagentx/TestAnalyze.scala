/*
 *  TestAnalyze.scala
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

import de.sciss.file._
import de.sciss.mutagentx.visual.Visual

import scala.swing.event.ButtonClicked
import scala.swing.{FlowPanel, ToggleButton, Button, BorderPanel, Frame, Swing}
import Swing._

object TestAnalyze extends App {
  run()

  def run(): Unit = {
    val base = file("database")
    require(base.isDirectory)
    val a = Algorithm(base / "test")
    val csr = a.global.cursor
    val v = csr.step { implicit tx => Visual(a) }

    Swing.onEDT {
      guiInit(v)
    }
  }

  def guiInit(v: Visual): Unit = {
    val ggPrevIter = Button("Prev Iter") {
      v.previousIteration()
    }

    val ggRunAnim = new ToggleButton("Run Animation") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          v.runAnimation = selected
      }
    }

    val ggStepAnim = Button("Step Anim") {
      v.animationStep()
    }

    val pBottom = new FlowPanel(ggPrevIter, ggRunAnim, ggStepAnim)

    new Frame {
      title     = "MutagenTx"
      contents  = new BorderPanel {
        add(v.component, BorderPanel.Position.Center)
        add(pBottom    , BorderPanel.Position.South)
      }
      pack()
      // size      = (640, 480)

      // v.display.panTo((-320, -240))
      v.display.panTo((0, 0))

      open()

      override def closeOperation(): Unit = {
        v.algorithm.system.close()
        sys.exit(0)
      }
    }

    //
    //    csr.step { implicit tx =>
    //      implicit val dtx = tx.durable
    //      println(s"path size = ${csr.position.size}")
    //      // a.print()
    //    }
    //    a.system.close()
  }
}
