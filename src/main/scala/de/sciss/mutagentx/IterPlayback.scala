/*
 *  IterPlayback.scala
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

import javax.swing.event.TableModelListener
import javax.swing.table.TableModel
import javax.swing.{JTable, SpinnerNumberModel}

import com.alee.laf.WebLookAndFeel
import de.sciss.audiowidgets.Transport
import de.sciss.desktop.{DialogSource, FileDialog, FocusType, KeyStrokes, OptionPane}
import de.sciss.file._
import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.swing.defer
import de.sciss.mutagentx.impl.EvaluationImpl
import de.sciss.serial.DataInput
import de.sciss.swingplus.{DoClickAction, Spinner}
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.synth.ugen.ConfigOut
import de.sciss.synth.{Server, ServerConnection, Synth, SynthDef, SynthGraph}
import de.sciss.{desktop, kollflitz, numbers, pdflitz, processor}
import org.jfree.chart.ChartPanel

import scala.collection.breakOut
import scala.concurrent.{Future, blocking}
import scala.swing.event.Key
import scala.swing.{BorderPanel, Button, Component, FlowPanel, Frame, Label, MainFrame, ScrollPane, Swing, Table}
import scala.util.{Failure, Success, Try}

object IterPlayback {
  case class Config(targetFile: File = file(""))

  def main(args: Array[String]): Unit = {
    ConfigOut.PAN2 = true
    ConfigOut.LIMITER /* CLIP */ = true
    ConfigOut.AMP = true

    val parser = new scopt.OptionParser[Config]("IterPlayback") {
      opt[File]('t', "target") required() text "target audio file" action { (x, c) => c.copy(targetFile = x) }
    }
    parser.parse(args, Config()).fold(sys.exit(1)) { cfg =>
      import cfg._
      val inputSpec = AudioFile.readSpec(targetFile)
      val sync = new AnyRef
      new Thread {
        override def run() = sync.synchronized(sync.wait())
        start()
      }
      Swing.onEDT {
        sync.synchronized(sync.notifyAll())
        WebLookAndFeel.install()
        // UIManager.setLookAndFeel("javax.swing.plaf.nimbus.NimbusLookAndFeel")
        guiInit(inputFile = targetFile, inputSpec = inputSpec)
      }
    }
  }

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

  def guiInit(inputFile: File, inputSpec: AudioFileSpec): Unit = {
    var synthOpt      = List.empty[Synth]
    var synthGraphOpt = Option.empty[SynthGraph]
    var lastFile      = Option.empty[File]
    var graphs        = Vec.empty[SOMGenerator.Input]
    var busy          = Option.empty[Future[Any]]

    val ggTable   = new Table() {
      override lazy val peer: JTable = new JTable /* with Table.JTableMixin */ with SuperMixin
    }
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
              val data: Array[(Int, Int, String, Float)] = sq.zipWithIndex.map { case (input, i) =>
                val sources = input.graph.sources
                val elems = sources.map(_.productPrefix).filterNot(hideUGens.contains).distinct.mkString(", ")
                // Array[AnyRef](i.asInstanceOf[AnyRef], sources.size.asInstanceOf[AnyRef], elems, input.fitness.asInstanceOf[AnyRef])
                (i, sources.size, elems, input.fitness)
              } (breakOut)

              val columnNames = Array("Index", "Num", "Elements", "Fit")

              val m1 = new TableModel {
                def getRowCount: Int = data.length

                def getColumnClass(columnIndex: Int): Class[_] = data(0).productElement(columnIndex).getClass

                def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false

                def getColumnCount: Int = columnNames.length

                def getColumnName(columnIndex: Int): String = columnNames(columnIndex)

                def addTableModelListener   (l: TableModelListener): Unit = ()
                def removeTableModelListener(l: TableModelListener): Unit = ()

                def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
                  val row = data(rowIndex)
                  row.productElement(columnIndex).asInstanceOf[AnyRef]
                }

                def setValueAt(aValue: scala.Any, rowIndex: Int, columnIndex: Int): Unit = ()
              }

              ggTable.model = m1
              ggTable.peer.setAutoCreateRowSorter(true)
            }
          }
        }
      }
    }

    import de.sciss.synth.Ops._

    def selectedGraphs(): List[SynthGraph] = ggTable.selection.rows.map { row => graphs(row).graph } (breakOut)

    def stopSynth(): Unit = {
      synthOpt.foreach { synth =>
        if (synth.server.isRunning) synth.free() // synth.release(3.0) // free()
      }
      synthOpt = Nil
    }

    val inMemory  = InMemory()
    type I        = InMemory

    // apply 'ranges'
    def mkGraph(in: SynthGraph): SynthGraph =
      inMemory.step { implicit tx =>
        val c0 = impl.ChromosomeImpl.mkChromosome[I](in)
        MkSynthGraph[I](c0, mono = false, removeNaNs = true, config = true, ranges = true)
      }

    def playSynth(): Unit = {
      stopSynth()
      Try(Server.default).toOption.foreach { s =>
        val graphs = selectedGraphs()
        synthGraphOpt = graphs.headOption
        val amp    = 1.0 / graphs.size
        val synths = graphs.map { graph0 =>
          val graph = mkGraph(graph0)
          val df    = SynthDef("test", graph.expand(DefaultUGenGraphBuilderFactory))
          val syn   = df.play(s, args = Seq("amp" -> amp))
          syn
        }
        synthOpt = synths
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
      synthGraphOpt.foreach { graph0 =>
        val graph = mkGraph(graph0)
        val x     = MkGraphSource(graph)
        println(x)
      }
    }

    val mBounceDur  = new SpinnerNumberModel(inputSpec.numFrames/inputSpec.sampleRate, 0.0, 3600.0, 0.1)
    val ggBounceDur = new Spinner(mBounceDur)
    val pBounceDur  = new FlowPanel(new Label("Duration [s]:"), ggBounceDur)
    var fBounce     = Option.empty[File]

    val ggBounce = Button("Bounce…") {
      selectedGraphs().headOption.foreach { graph =>
        val opt = OptionPane(message = pBounceDur, optionType = OptionPane.Options.OkCancel, focus = Some(ggBounceDur))
        if (opt.show(None) == OptionPane.Result.Ok) {
          val fDlg = FileDialog.save(fBounce, title = "Bounce Synth Graph")
          fDlg.show(None).foreach { f =>
            fBounce = Some(f)
            val fut = EvaluationImpl.bounce(graph = graph, audioF = f, inputSpec = inputSpec,
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

    val ggSimilarity = Button("Compare") {
      selectedGraphs() match {
        case a :: b :: _ =>
          import Algorithm.executionContext
          val fut = Future(blocking(EvaluationImpl.graphSimilarity(a, b)))
          fut.onComplete {
            case Success(v) => println(f"Similarity between the two graphs: ${v * 100}%1.1f%%.")
            case Failure(ex) => ex.printStackTrace()
          }

        case _ => println("Must select two rows!")
      }
    }

    val ggSimHisto = Button("Sim Histo") {
      import Algorithm.executionContext
      import processor._
      val dataFut = Processor[Vec[Double]]("calc-histo") { self =>
        blocking {
          val N = graphs.size
          graphs.zipWithIndex.map { case (a, ai) =>
            val simSum = (0.0 /: graphs) { case (res, b) =>
              if (a eq b) res else res + EvaluationImpl.graphSimilarity(a.graph, b.graph)
            }
            self.progress = (ai + 1).toDouble / N
            self.checkAborted()
            simSum / (N - 1)
          }
        }
      }

      dataFut.monitor(printResult = false)

      val accumFut = dataFut.map { data =>
        val min     = data.min
        val max     = data.max
        val numBins = 400
        val histo   = new Array[Int](numBins)
        import numbers.Implicits._
        data.foreach { d =>
          val bin = d.linlin(min, max, 0, numBins).toInt.min(numBins - 1)
          histo(bin) += 1
        }
        val size      = (0L /: histo)((sum, count) => sum + count)
        val relative: Vec[Double] = histo.map(count => (count * 100.0)/ size)(breakOut)
        import kollflitz.Ops._
        val accum     = relative.integrate
        (accum, min, max)
      }

      accumFut.onSuccess { case (accum, min, max) =>
        defer {
          val chart = Util.mkHistogramChart(accum, xMin = min, xMax = max,
            title = "Graph Similarity Accumulative Histogram")
          val pj = new ChartPanel(chart.peer, false)
          val p  = Component.wrap(pj)
          val f  = new Frame {
            contents = p
            new pdflitz.SaveAction(List(p)).setupMenu(this)
            pack().centerOnScreen()
            open()
          }
        }
      }
    }

    import desktop.Implicits._
    val ggPlay = bs.button(Transport.Play).get
    val acPlay = DoClickAction(ggPlay)
    acPlay.accelerator = Some(KeyStrokes.menu1 + Key.Enter)
    ggPlay.addAction("click", acPlay, FocusType.Window)

    val tp = new FlowPanel(ggOpen, pStatus, butKill, bs, ggPrint, ggBounce, ggSimilarity, ggSimHisto)

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