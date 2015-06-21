package de.sciss.mutagentx

import javax.swing.table.DefaultTableModel

import com.alee.laf.WebLookAndFeel
import de.sciss.audiowidgets.Transport
import de.sciss.desktop.{DialogSource, FileDialog}
import de.sciss.file._
import de.sciss.lucre.swing.defer
import de.sciss.serial.DataInput
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.synth.ugen.ConfigOut
import de.sciss.synth.{Server, ServerConnection, Synth, SynthDef, SynthGraph}

import scala.collection.breakOut
import scala.concurrent.{Future, blocking}
import scala.swing.{SwingApplication, BorderPanel, Button, FlowPanel, MainFrame, ScrollPane, Table}
import scala.util.Try

object IterPlayback extends SwingApplication {
  def startup(args: Array[String]): Unit = {
    WebLookAndFeel.install()
    ConfigOut.PAN2 = true
    guiInit()
  }

  def guiInit(): Unit = {
    var synthOpt      = Option.empty[Synth]
    var synthGraphOpt = Option.empty[SynthGraph]
    var lastFile      = Option.empty[File]
    var graphs        = Vec.empty[SOMGenerator.Input]
    var busy          = Option.empty[Future[Any]]

    def open(f: File): Vec[SOMGenerator.Input] = {
      val in = DataInput.open(f)
      try {
        val b   = Vec.newBuilder[SOMGenerator.Input]
        val ser = SOMGenerator.Input.serializer
        while (in.position < in.size) {
          val input = ser.read(in)
          b += input
        }
        b.result()
      } finally {
        in.close()
      }
    }

    val ggTable   = new Table()
    val ggScroll  = new ScrollPane(ggTable)

    val ggOpen = Button("Open...") {
      val fInit = lastFile.getOrElse(file("database").absolute)
      if (busy.isEmpty) {
        val fDlg = FileDialog.open(init = Some(fInit))
        fDlg.setFilter(_.ext.toLowerCase == "bin")
        fDlg.show(None).foreach { f =>
          lastFile = Some(f)
          import Algorithm.executionContext
          val fut = Future(blocking(open(f)))
          busy    = Some(fut)
          fut.onComplete { case _ => defer { busy = None }}
          fut.onFailure {
            case ex => defer {
              DialogSource.Exception(ex -> "Open SynthGraph File").show(None)
            }
          }
          fut.onSuccess {
            case sq => defer {
              graphs = sq
              val data: Array[Array[AnyRef]] = sq.zipWithIndex.map { case (input, i) =>
                val elems = input.graph.sources.map(_.productPrefix).filterNot(_ == "RandSeed").distinct.mkString(", ")
                Array[AnyRef](i.asInstanceOf[AnyRef], elems, input.fitness.asInstanceOf[AnyRef])
              } (breakOut)

              ggTable.peer.setAutoCreateRowSorter(true)
              ggTable.model = new DefaultTableModel(data, Array[AnyRef]("Index", "Elements", "Fit"))
            }
          }
        }
      }
    }

    import de.sciss.synth.Ops._

    def stopSynth(): Unit = synthOpt.foreach { synth =>
      synthOpt = None
      if (synth.server.isRunning) synth.free() // synth.release(3.0) // free()
    }
    def playSynth(): Unit = {
      stopSynth()
      for {
        s      <- Try(Server.default).toOption
        selRow <- ggTable.selection.rows.headOption
      } {
        val graph = graphs(selRow).graph
        val df    = SynthDef("test", graph.expand(DefaultUGenGraphBuilderFactory))
        val x     = df.play(s, args = Seq("out" -> 0))
        synthOpt      = Some(x)
        synthGraphOpt = Some(graph)
      }
    }

    val pStatus = new ServerStatusPanel
    def boot(): Unit = {
      val cfg = Server.Config()
      cfg.memorySize = 256 * 1024
      cfg.pickPort()
      val connect = Server.boot(config = cfg) {
        case ServerConnection.Running(s) =>
        case ServerConnection.Aborted    =>
      }
      pStatus.booting = Some(connect)
    }

    val butKill = Button("Kill") {
      import scala.sys.process._
      Try(Server.default).toOption.foreach(_.dispose())
      "killall scsynth".!
    }

    pStatus.bootAction = Some(boot)
    val bs = Transport.makeButtonStrip(Seq(Transport.Stop(stopSynth()), Transport.Play(playSynth())))
    val ggPrint = Button("Print") {
      synthGraphOpt.foreach { graph =>
        val x = impl.ChromosomeImpl.graphToString(graph)
        println(x)
      }
    }
    val tp = new FlowPanel(ggOpen, pStatus, butKill, bs, ggPrint)

    new MainFrame {
      contents = new BorderPanel {
        add(tp      , BorderPanel.Position.North )
        add(ggScroll, BorderPanel.Position.Center)
      }
      pack().centerOnScreen()
      open()
    }
  }
}