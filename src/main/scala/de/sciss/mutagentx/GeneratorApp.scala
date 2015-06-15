package de.sciss.mutagentx

import com.alee.laf.WebLookAndFeel
import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.swing.defer

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}
import scala.swing.{BorderPanel, Button, CheckBox, FlowPanel, Frame, Label, SwingApplication, TextField}
import scala.util.control.NonFatal

object GeneratorApp extends SwingApplication {
  type S = ConfluentReactive

  def startup(args: Array[String]): Unit = {
    WebLookAndFeel.install()

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

    def updateStats(a: Algorithm[S]): Unit = {
      val csr = a.global.cursor
      val (fit: Vec[Float], inp: S#Acc) = csr.step { implicit tx =>
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

    def init(): Future[Algorithm[S]] = Future {
      val dir = file("database"  ) / (if (args.length > 0) args(0) else "betanovuss")
      val in  = file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
      val algorithm = blocking(Algorithm(dir = dir, input = in))

      val cursor = algorithm.global.cursor
      val isNew = cursor.step { implicit tx =>
        val _isNew = algorithm.genome.chromosomes().isEmpty
        if (_isNew) blocking(algorithm.init(Algorithm.population))
        _isNew
      }
      if (isNew) {
        val fut0 = cursor.step { implicit tx =>
          algorithm.evaluateAndUpdate()
        }
        Await.result(fut0, Duration.Inf)
      }
      algorithm
    }

    def iter(a: Algorithm[S]): Future[Unit] = Future {
      val peer = a.iterate()
      Await.result(peer, Duration.Inf)
    }
//      .andThen { case Success(_) =>
//        a.global.cursor.step { implicit tx =>
//          a.evaluateAndUpdate()
//        }
//      }

    val ggBusy = new CheckBox
    var busy = Option.empty[Future[Any]]

    def setBusy(fut: Future[Any]): Unit = {
      busy = Some(fut)
      ggBusy.selected = true
      fut.onComplete {
        case _ => defer {
          busy = None
          ggBusy.selected = false
        }
      }
      fut.onFailure {
        case ex => ex.printStackTrace()
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
          val fut = Future {
            for (i <- 1 to num) {
              println(s"-------------STEP $i-------------")
              val futI = iter(a)
              Await.result(futI, Duration.Inf) // Duration(60, TimeUnit.SECONDS))
            }
          }
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

    val pBot = new FlowPanel(ggInit, ggStep1, ggStepN, ggStepNum, ggBusy)

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
