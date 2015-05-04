package de.sciss.mutagentx
package visual

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.processor.Processor
import prefuse.util.ui.JForcePanel

import scala.swing.Swing._
import scala.swing.event.ButtonClicked
import scala.swing.{Component, Orientation, SplitPane, BorderPanel, Button, FlowPanel, Frame, ProgressBar, Swing, ToggleButton}

object VertexApp extends App {
  run()

  def run(): Unit = {
    val dir = file("database") / "betanovuss"
    val in  = file("audio_work") / "Betanovuss150410_1Cut.aif"
    val a   = Algorithm(dir = dir, input = in)
    val csr = a.global.cursor
    val v   = csr.step { implicit tx => Visual(a) }

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
      val fmt   = new SimpleDateFormat("'betanovuss_'yyMMdd'_'HHmmss'.png'", Locale.US)
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
      v.display.panTo((0, 0))

      open()

      override def closeOperation(): Unit = {
        v.algorithm.system.close()
        sys.exit(0)
      }
    }
  }
}
