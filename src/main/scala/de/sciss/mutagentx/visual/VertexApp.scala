/*
 *  VertexApp.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.lucre.stm.Sys
import de.sciss.processor.Processor
import prefuse.util.ui.JForcePanel

import scala.swing.Swing._
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Component, FlowPanel, Frame, Orientation, ProgressBar, SplitPane, Swing, ToggleButton}
import scala.util.{Failure, Success}

object VertexApp extends App {
  run()

  def run(): Unit = {
    val dir = file("database"  ) / (if (args.length > 0) args(0) else "betanovuss")
    val in  = file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
    val a   = Algorithm.confluent(???) // dir = dir, input = in)
    val csr = a.global.cursor
    val v   = csr.step { implicit tx => Visual(a) }

    Swing.onEDT {
      guiInit(v)
    }
  }

  def guiInit[S <: Sys[S]](v: Visual[S]): Unit = {
    val ggHead = Button("Head") {
      v.initChromosome(100)
    }

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
      val fmt   = new SimpleDateFormat("'betanovuss_'yyMMdd'_'HHmmss'.png'", Locale.US)
      val name  = fmt.format(new Date)
      val f     = userHome / "Pictures" / name
      val vs    = VideoSettings()
      v.saveFrameAsPNG(f, width = vs.width, height = vs.height)
    }

    var seriesProc = Option.empty[Processor[Unit]]

    val ggProgress = new ProgressBar

    val ggSaveFrameSeries = Button("Save Movie...") {
      seriesProc.fold[Unit] {
        val dir       = file("render")
        require(dir.isDirectory)
        val cfg       = VideoSettings()
        cfg.secondsSkip  = 0.0 // 60
        cfg.secondsDecay = 20.0 // 60.0 //
        cfg.secondsPerIteration = 10.0 // 2.0
        cfg.chromosomeIndex     = 100
        cfg.baseFile  = dir / "frame"
        val p         = v.saveFrameSeriesAsPNG(cfg)
        seriesProc    = Some(p)
        p.addListener {
          case prog @ Processor.Progress(_, _) => onEDT(ggProgress.value = prog.toInt)
          case Processor.Result(_, Success(_)) => println("Done.")
          case Processor.Result(_, Failure(ex)) =>
            println("Move rendering failed.")
            ex.printStackTrace()
        }

      } { p =>
        p.abort()
        seriesProc = None
      }
    }

    val pBottom = new FlowPanel(ggHead, ggPrevIter, ggRunAnim, ggStepAnim, ggSaveFrame, ggSaveFrameSeries, ggProgress)

    val fSim    = v.forceSimulator
    val fPanel  = new JForcePanel(fSim)
    fPanel.setBackground(null)
    val split = new SplitPane(Orientation.Vertical, v.component, Component.wrap(fPanel))
    split.oneTouchExpandable  = true
    split.continuousLayout    = false
    split.dividerLocation     = 800
    split.resizeWeight        = 1.0

    new Frame {
      title     = "MutagenTx"
      contents  = new BorderPanel {
        add(split   , BorderPanel.Position.Center)
        add(pBottom , BorderPanel.Position.South)
      }
      pack()
      // size      = (640, 480)

      // v.display.panTo((-320, -240))
      // v.display.panTo((0, 0))
      v.display.panTo((-136 + 20, -470 + 20))   // XXX WAT -- where the heck do these values come from?
      v.display.zoomAbs((0, 0), 1.3333)

      open()

      override def closeOperation(): Unit = {
        try {
          v.algorithm.close()
        } finally {
          sys.exit(0)
        }
      }
    }
  }
}
