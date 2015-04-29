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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.mutagentx.visual.{VideoSettings, Visual}
import de.sciss.processor.Processor

import scala.swing.event.ButtonClicked
import scala.swing.{ProgressBar, FlowPanel, ToggleButton, Button, BorderPanel, Frame, Swing}
import Swing._

object TestAnalyze extends App {
  run()

  def run(): Unit = {
    val base = file("database")
    require(base.isDirectory)
    val a = AlgorithmOLD(base / "test")
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

    val ggSaveFrame = Button("Save Frame") {
      val fmt   = new SimpleDateFormat("'mutagentx_'yyMMdd'_'HHmmss'.png'", Locale.US)
      val name  = fmt.format(new Date)
      val f     = userHome / "Pictures" / name
      v.saveFrameAsPNG(f)
    }

    var seriesProc = Option.empty[Processor[Unit]]

    val ggProgress = new ProgressBar

    val ggSaveFrameSeries = Button("Save Movie...") {
      seriesProc.fold[Unit] {
        val dir       = file("render")
        require(dir.isDirectory)
        val cfg       = VideoSettings()
        cfg.secondsSkip = 60
        cfg.secondsDecay = 60
        cfg.baseFile  = dir / "movie"
        val p         = v.saveFrameSeriesAsPNG(cfg)
        seriesProc    = Some(p)
        p.addListener {
          case prog @ Processor.Progress(_, _) => onEDT(ggProgress.value = prog.toInt)
        }

      } { p =>
        p.abort()
        seriesProc = None
      }
    }

    val pBottom = new FlowPanel(ggPrevIter, ggRunAnim, ggStepAnim, ggSaveFrame, ggSaveFrameSeries, ggProgress)

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
