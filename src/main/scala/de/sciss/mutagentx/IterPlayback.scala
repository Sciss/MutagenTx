package de.sciss.mutagentx

import javax.swing.SpinnerNumberModel
import javax.swing.table.DefaultTableModel

import com.alee.laf.WebLookAndFeel
import de.sciss.audiowidgets.Transport
import de.sciss.desktop
import de.sciss.desktop.{KeyStrokes, DialogSource, FileDialog, FocusType, OptionPane}
import de.sciss.file._
import de.sciss.lucre.swing.defer
import de.sciss.serial.DataInput
import de.sciss.swingplus.{DoClickAction, Spinner}
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.synth.ugen.{ConfigOut, Out}
import de.sciss.synth.{Server, ServerConnection, Synth, SynthDef, SynthGraph}

import scala.collection.breakOut
import scala.concurrent.{Future, blocking}
import scala.swing.event.Key
import scala.swing.{BorderPanel, Button, FlowPanel, Label, MainFrame, ScrollPane, Swing, Table}
import scala.util.{Failure, Success, Try}

object IterPlayback {
  case class Config(targetFile: File = file(""))

  def main(args: Array[String]): Unit = {
    ConfigOut.PAN2 = true
    ConfigOut.LIMITER /* CLIP */ = true

    val parser = new scopt.OptionParser[Config]("IterPlayback") {
      opt[File]('t', "target") required() text "target audio file" action { (x, c) => c.copy(targetFile = x) }
    }
    parser.parse(args, Config()).fold(sys.exit(1)) { cfg =>
      import cfg._
      val inputSpec = AudioFile.readSpec(targetFile)
      Swing.onEDT {
        WebLookAndFeel.install()
        guiInit(inputFile = targetFile, inputSpec = inputSpec)
      }
    }
  }

  def guiInit(inputFile: File, inputSpec: AudioFileSpec): Unit = {
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

    val hideUGens = Set[String]("RandSeed", "Mix", "Mix$Mono", "ConfigOut")

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
                val sources = input.graph.sources
                val elems = sources.map(_.productPrefix).filterNot(hideUGens.contains).distinct.mkString(", ")
                Array[AnyRef](i.asInstanceOf[AnyRef], sources.size.asInstanceOf[AnyRef], elems, input.fitness.asInstanceOf[AnyRef])
              } (breakOut)

              ggTable.peer.setAutoCreateRowSorter(true)
              ggTable.model = new DefaultTableModel(data, Array[AnyRef]("Index", "Num", "Elements", "Fit"))
            }
          }
        }
      }
    }

    import de.sciss.synth.Ops._

    def selectedGraph(): Option[SynthGraph] = ggTable.selection.rows.headOption.map { row => graphs(row).graph }

    def stopSynth(): Unit = synthOpt.foreach { synth =>
      synthOpt = None
      if (synth.server.isRunning) synth.free() // synth.release(3.0) // free()
    }

    def playSynth(): Unit = {
      stopSynth()
      for {
        s      <- Try(Server.default).toOption
        graph0 <- selectedGraph()
      } {
        val graph = graph0
//          .copy(sources = graph0.sources.collect {
//            case ConfigOut(in) => Out.ar(0, in)
//            case x => x
//          })
        val df    = SynthDef("test", graph.expand(DefaultUGenGraphBuilderFactory))
        val syn   = df.play(s, args = Seq("out" -> 0))
        synthOpt      = Some(syn)
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

    val mBounceDur  = new SpinnerNumberModel(inputSpec.numFrames/inputSpec.sampleRate, 0.0, 3600.0, 0.1)
    val ggBounceDur = new Spinner(mBounceDur)
    val pBounceDur  = new FlowPanel(new Label("Duration [s]:"), ggBounceDur)
    var fBounce     = Option.empty[File]

    val ggBounce = Button("Bounce…") {
      selectedGraph().foreach { graph =>
        val opt = OptionPane(message = pBounceDur, optionType = OptionPane.Options.OkCancel, focus = Some(ggBounceDur))
        if (opt.show(None) == OptionPane.Result.Ok) {
          val fDlg = FileDialog.save(fBounce, title = "Bounce Synth Graph")
          fDlg.show(None).foreach { f =>
            fBounce = Some(f)
            val fut = impl.EvaluationImpl.bounce(graph = graph, audioF = f, inputSpec = inputSpec,
              duration0 = mBounceDur.getNumber.doubleValue())
            import Algorithm.executionContext
            fut.onComplete {
              case Success(_) => println("Done.")
              case Failure(ex) => DialogSource.Exception(ex -> "Bounce").show(None)
            }
          }
        }
      }
    }

    import desktop.Implicits._
    val ggPlay = bs.button(Transport.Play).get
    val acPlay = DoClickAction(ggPlay)
    acPlay.accelerator = Some(KeyStrokes.menu1 + Key.Enter)
    ggPlay.addAction("click", acPlay, FocusType.Window)

    val tp = new FlowPanel(ggOpen, pStatus, butKill, bs, ggPrint, ggBounce)

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