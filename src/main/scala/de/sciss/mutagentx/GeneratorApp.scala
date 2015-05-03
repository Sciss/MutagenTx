package de.sciss.mutagentx

import java.util.concurrent.TimeUnit

import com.alee.laf.WebLookAndFeel
import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.lucre.swing.defer

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.swing.{Button, BorderPanel, FlowPanel, TextField, Label, Frame, SwingApplication}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object GeneratorApp extends SwingApplication {
  def startup(args: Array[String]): Unit = {
    WebLookAndFeel.install()

    val ggBest = new TextField(4) {
      editable = false
    }
    val ggAvg  = new TextField(4) {
      editable = false
    }
    val ggMedian = new TextField(4) {
      editable = false
    }
    val ggIter = new TextField(4) {
      editable = false
    }
    val pTop = new FlowPanel(
      new Label("Best:"  ), ggBest,
      new Label("Avg:"   ), ggAvg,
      new Label("Median:"), ggMedian,
      new Label("Iter:"  ), ggIter
    )

    var algorithm = Option.empty[Algorithm]
    import Algorithm.executionContext

    def updateStats(a: Algorithm): Unit = {
      val csr = a.global.cursor
      val (fit, inp) = csr.step { implicit tx =>
        implicit val dtx = tx.durable
        a.genome.fitness() -> csr.position
      }
      import kollflitz.Ops._
      val sorted = fit.sortedT
      ggBest  .text = f"${sorted.last  }%1.3f"
      ggAvg   .text = f"${sorted.mean  }%1.3f"
      ggMedian.text = f"${sorted.median}%1.3f"
      ggIter  .text = (inp.size / 2).toString
    }

    def init(): Future[Algorithm] = {
      val dir = file("database") / "betanovuss"
      val in  = file("audio_work") / "Betanovuss150410_1Cut.aif"
      val algorithm = Algorithm(dir = dir, input = in)

      val cursor = algorithm.global.cursor
      val isNew = cursor.step { implicit tx =>
        val _isNew = algorithm.genome.chromosomes().isEmpty
        if (_isNew) algorithm.init(100)
        _isNew
      }
      if (isNew) {
        val fut0 = cursor.step { implicit tx =>
          algorithm.evaluateAndUpdate()
        }
        fut0.map(_ => algorithm)
      } else {
        Future.successful(algorithm)
      }
    }

    def iter(a: Algorithm): Future[Unit] =
      a.iterate().andThen { case Success(_) =>
        a.global.cursor.step { implicit tx =>
          a.evaluateAndUpdate()
        }
      }

    val ggInit = Button("New/Open") {
      val fut = init()
      fut.onComplete {
        case Success(a) =>
          defer {
            algorithm = Some(a)
            updateStats(a)
          }
        case Failure(ex) =>
          ex.printStackTrace()
      }
    }
    val ggStep1 = Button("Step 1") {
      algorithm.foreach { a =>
        val fut = iter(a)
        fut.onComplete {
          case Success(_) =>
            defer {
              updateStats(a)
            }
          case Failure(ex) =>
            ex.printStackTrace()
        }
      }
    }
    val ggStepNum = new TextField(3) {
      text = 100.toString
    }
    val ggStepN = Button("Step N") {
      val num = math.max(1, ggStepNum.text.toInt)
      algorithm.foreach { a =>
        val fut = Future {
          for (i <- 1 to num) {
            println(s"-------------STEP $i-------------")
            val futI = iter(a)
            Await.result(futI, Duration(60, TimeUnit.SECONDS))
          }
        }
        fut.onComplete {
          case Success(_) =>
            defer {
              updateStats(a)
            }
          case Failure(ex) =>
            ex.printStackTrace()
        }
      }
    }

    val pBot = new FlowPanel(ggInit, ggStep1, ggStepN, ggStepNum)

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
}
