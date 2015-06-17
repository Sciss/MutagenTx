package de.sciss.mutagentx

import java.awt.Color

import com.alee.laf.WebLookAndFeel
import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys
import de.sciss.lucre.swing.defer
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}
import scala.swing.{BorderPanel, Button, FlowPanel, Frame, Label, ProgressBar, SwingApplication, TextField}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object GeneratorApp extends SwingApplication {
  type S = ConfluentReactive

  def startup(args: Array[String]): Unit = {
    WebLookAndFeel.install()
    args.toIndexedSeq match {
      case "--confluent" +: tail =>
        new Confluent(tail)

      case "--in-memory" +: sfPath +: _ =>
      case _ =>
        Console.err.println(
          """Invocation:
            |
            |--confluent [<database-name> <sf-name>]
            |--in-memory <sf-name>
            |""".stripMargin)
        sys.exit(1)
    }
  }

  final class Confluent(args: Vec[String]) extends GenApp[ConfluentReactive] {
    type S = ConfluentReactive

    def getIteration(a: Algorithm[S]): Int = {
      val csr = a.global.cursor
      val inp = csr.step { implicit tx =>
        implicit val dtx = tx.durable
        csr.position
      }
      inp.size / 2
    }

    def init(): Processor[Algorithm[S]] = {
      import Algorithm.executionContext
      val res = new Init
      res.start()
      res
    }

    private final class Init
      extends ProcessorImpl[Algorithm[S], Processor[Algorithm[S]]] with Processor[Algorithm[S]] {

      protected def body(): Algorithm[S] = {
        val dir = file("database"  ) / (if (args.length > 0) args(0) else "betanovuss")
        val in  = file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
        val algorithm = blocking(Algorithm.confluent(dir = dir, input = in))

        val cursor = algorithm.global.cursor
        val isNew = cursor.step { implicit tx =>
          val _isNew = algorithm.genome.chromosomes().isEmpty
          if (_isNew) {
            val futInit = algorithm.initialize(Algorithm.population)
            await(futInit, 0, 0.5)
          }
          _isNew
        }
        if (isNew) {
          val fut0 = cursor.step { implicit tx =>
            algorithm.evaluateAndUpdate()
          }
          await(fut0, 0.5, 0.5)
        }
        algorithm
      }
    }
  }
}
trait GenApp[S <: Sys[S]] {
  def mkNumView() = {
    val t = new TextField(4)
    t.editable  = false
    t.focusable = false
    t
  }

  val ggBest    = mkNumView()
  val ggAvg     = mkNumView()
  val ggMedian  = mkNumView()
  val ggIter    = mkNumView()

  val pTop = new FlowPanel(
    new Label("Best:"  ), ggBest,
    new Label("Avg:"   ), ggAvg,
    new Label("Median:"), ggMedian,
    new Label("Iter:"  ), ggIter
  )

  var algorithm = Option.empty[Algorithm[S]]
  import Algorithm.executionContext

  def getIteration(a: Algorithm[S]): Int

  def updateStats(a: Algorithm[S]): Unit = {
    val csr = a.global.cursor
    val fit: Vec[Float] = csr.step { implicit tx => a.genome.fitness() }
    import kollflitz.Ops._
    val sorted = fit.sortedT
    ggBest  .text = f"${sorted.last  }%1.3f"
    ggAvg   .text = f"${sorted.mean  }%1.3f"
    ggMedian.text = f"${sorted.median}%1.3f"
    ggIter  .text = getIteration(a).toString
  }

  def init(): Processor[Algorithm[S]]

  def iter(a: Algorithm[S]): Processor[Unit] = a.iterate()

//      .andThen { case Success(_) =>
//        a.global.cursor.step { implicit tx =>
//          a.evaluateAndUpdate()
//        }
//      }

  val ggProgress = new ProgressBar
  var busy = Option.empty[Processor[Any]]

  val ggAbort = Button("X") {
    busy.foreach(_.abort())
  }
  ggAbort.enabled = false

  def setBusy(fut: Processor[Any]): Unit = {
    busy = Some(fut)
    ggProgress.background = Color.yellow
    ggAbort.enabled = false

    fut.onComplete {
      case _ => defer {
        busy = None
        ggAbort.enabled = true
      }
    }

    fut.onComplete {
      case Success(_) => defer {
        ggProgress.value = 100
        ggProgress.background = new Color(0x00, 0xA0, 0x00)
      }

      case Failure(Processor.Aborted()) => defer {
        ggProgress.value = 0
        ggProgress.background = Color.gray
      }

      case Failure(ex) => defer {
        ggProgress.value = 0
        ggProgress.background = Color.red
        ex.printStackTrace()
      }
    }
  }

  val ggInit = Button("New/Open") {
    if (busy.isEmpty) {
      val fut = init()
      fut.onSuccess {
        case a =>
          defer {
            algorithm = Some(a)
            updateStats(a)
          }
      }
      setBusy(fut)
    }
  }
  val ggStep1 = Button("Step 1") {
    if (busy.isEmpty) {
      algorithm.foreach { a =>
        val t0  = System.currentTimeMillis()
        val fut = iter(a)
        fut.onSuccess { case _ =>
          val t1 = System.currentTimeMillis()
          println(f"---------TOOK ${(t1-t0)*0.001}%1.3f sec.---------")
          defer {
            updateStats(a)
          }
        }
        setBusy(fut)
      }
    }
  }
  val ggStepNum = new TextField(3) {
    text = 100.toString
  }
  val ggStepN = Button("Step N") {
    if (busy.isEmpty) {
      val num = math.max(1, ggStepNum.text.toInt)
      algorithm.foreach { a =>
        val t0  = System.currentTimeMillis()
        val fut = new StepN(num, a)
        fut.start()
        fut.onSuccess { case _ =>
          val t1 = System.currentTimeMillis()
          println(f"---------TOOK ${(t1-t0)*0.001}%1.3f sec.---------")
          defer {
            updateStats(a)
          }
        }
        setBusy(fut)
      }
    }
  }

  private[this] final class StepN(num: Int, a: Algorithm[S])
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = {
      val weight = 1.0 / num
      for (i <- 0 until num) {
        // println(s"-------------STEP $i-------------")
        val futI = iter(a)
        await(futI, offset = i * weight, weight = weight)
        Await.result(futI, Duration.Inf) // Duration(60, TimeUnit.SECONDS))
      }
    }
  }


  val pBot = new FlowPanel(ggInit, ggStep1, ggStepN, ggStepNum, ggProgress, ggAbort)

  new Frame {
    title = "MutagenTx"
    contents = new BorderPanel {
      add(pTop, BorderPanel.Position.North)
      add(pBot, BorderPanel.Position.South)
    }
    pack().centerOnScreen()
    open()

    override def closeOperation(): Unit = {
      try { algorithm.foreach(_.system.close()) } catch { case NonFatal(ex) => ex.printStackTrace() }
      sys.exit()
    }
  }
}
