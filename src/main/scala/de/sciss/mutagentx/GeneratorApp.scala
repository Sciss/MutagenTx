package de.sciss.mutagentx

import java.awt.Color

import com.alee.laf.WebLookAndFeel
import com.alee.laf.checkbox.WebCheckBoxStyle
import com.alee.laf.progressbar.WebProgressBarStyle
import de.sciss.desktop.OptionPane
import de.sciss.file._
import de.sciss.mutagentx.Algorithm.SysType
import de.sciss.{mutagentx, kollflitz}
import de.sciss.lucre.confluent
import de.sciss.lucre.stm.{Sys, Durable, InMemory}
import de.sciss.lucre.swing.defer
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.ugen.ConfigOut
import scopt.OptionParser

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}
import scala.swing.{BorderPanel, Button, FlowPanel, Frame, Label, ProgressBar, SwingApplication, TextField}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object GeneratorApp extends SwingApplication {
  def startup(args: Array[String]): Unit = {
    mutagentx.init()

    WebLookAndFeel.install()
    WebCheckBoxStyle   .animated            = false
    WebProgressBarStyle.progressTopColor    = Color.lightGray
    WebProgressBarStyle.progressBottomColor = Color.gray
    // XXX TODO: how to really turn off animation?
    WebProgressBarStyle.highlightWhite      = new Color(255, 255, 255, 0)
    WebProgressBarStyle.highlightDarkWhite  = new Color(255, 255, 255, 0)

    // ConfigOut.CLIP = true
    ConfigOut.LIMITER = true

    val parser = new scopt.OptionParser[Algorithm.Config]("GeneratorApp") {
      opt[File]('t', "target").text("Search target audio file path").required().action {
        case (v, c) => c.copy(audioFile = v)
      }
      opt[File]('d', "database").text("Output database file").action { case (v, c) => c.copy(databaseFile = v) }
      opt[Unit]("in-memory").text("Select in-memory system type").action {
        case (v, c) => c.copy(tpe = SysType.InMemory) }
      opt[Unit]("durable").text("Select durable system type").action {
        case (v, c) => c.copy(tpe = SysType.Durable) }
      opt[Unit]("hybrid").text("Select hybrid durable system type (default)").action {
        case (v, c) => c.copy(tpe = SysType.Hybrid) }
      opt[Unit]("confluent").text("Select confluent system type").action {
        case (v, c) => c.copy(tpe = SysType.Confluent) }
      opt[Int]('p', "population").text("Population size").action { case (v, c) => c.copy(population = v) }
      opt[Double]("const-prob").text("Probability (0 to 1) of constants over UGen creation").action {
        case (v, c) => c.copy(constProb = v) }
      opt[Int]('m', "min-vertices").text("Minimum number of vertices").action {
        case (v, c) => c.copy(minNumVertices = v) }
      opt[Int]('x', "max-vertices").text("Maximum number of vertices").action {
        case (v, c) => c.copy(maxNumVertices = v) }
      opt[Double]("non-default-prob").text("Probability (0 to 1) of filling in default inlets").action {
        case (v, c) => c.copy(nonDefaultProb = v) }
      opt[Int]('c', "num-mfcc").text("Number of MFCC coefficients").action { case (v, c) => c.copy(numMFCC = v) }
      opt[Unit]('n', "norm-mfcc").text("Normalize MFCC coefficients").action {
        case (v, c) => c.copy(normalizeMFCC = true) }
      opt[Double]("non-default-prob").text("Probability (0 to 1) of filling in default inlets").action {
        case (v, c) => c.copy(nonDefaultProb = v) }
      opt[Double]("max-boost").text("Maximum amplitude boost allowed during cross-correlation").action {
        case (v, c) => c.copy(maxBoost = v) }
      opt[Double]('w', "temporal-weight")
        .text("Weighting between purely spectral (0) and purely temporal (1) features").action {
        case (v, c) => c.copy(temporalWeight = v) }
      opt[Double]("vertex-penalty").text("Penalty (0 to 1) for high number of vertices").action {
        case (v, c) => c.copy(vertexPenalty = v) }
      opt[Double]("graph-penalty").text("Amount of penalty for two graphs with maximum similarity").action {
        case (v, c) => c.copy(graphPenaltyAmt = v) }
      opt[Int]("graph-penalty-iter").text("Number of iterations between graph penalty application").action {
        case (v, c) => c.copy(graphPenaltyIter = v) }
      opt[Double]("graph-penalty-ceil").text("Maximum expected (clipped) inter-graph similarity").action {
        case (v, c) => c.copy(graphPenaltyCeil = v) }
      opt[Double]("graph-penalty-prob").text("Probability (0 to 1) of subjecting graph to similarity test").action {
        case (v, c) => c.copy(graphPenaltyCoin = v) }
      opt[Double]('s', "selection-frac").text("Fraction (0 to 1) of individuals selected for breeding").action {
        case (v, c) => c.copy(selectionFrac = v) }
      opt[Int]('e', "elitism").text("Number of elite individuals").action { case (v, c) => c.copy(numElitism = v) }
      opt[Int]("min-mut").text("Minimum mutations per individual").action { case (v, c) => c.copy(mutMin = v) }
      opt[Int]("max-mut").text("Maximum mutations per individual").action { case (v, c) => c.copy(mutMax = v) }
      opt[Double]('u', "mutation-prob").text("Probability (0 to 1) of mutation over crossover").action {
        case (v, c) => c.copy(mutationProb = v) }
      opt[Int]('g', "golem").text("Number of newborn individuals per iteration").action {
        case (v, c) => c.copy(numGolem = v) }
    }

    parser.parse(args, Algorithm.Config()).fold(sys.exit(1)) { config =>
      config.tpe match {
        case SysType.InMemory =>
          new InMemoryApp(config)
        case SysType.Durable =>
          new DurableApp(config)
        case SysType.Hybrid =>
          new DurableHybridApp(config)
        case SysType.Confluent =>
          new ConfluentApp(config)
      }
    }
  }

  final class InMemoryApp(config: Algorithm.Config) extends GenApp[InMemory] {
    type S = InMemory
    type A = Algorithm[InMemory]

    def init(): Processor[A] = {
      import Algorithm.executionContext
      val res = new Init
      res.start()
      res
    }

    private final class Init extends ProcessorImpl[A, Processor[A]] with Processor[A] {
      def body(): A = {
        val algorithm = Algorithm.inMemory(config)
        val fut1 = algorithm.global.cursor.step { implicit tx =>
          algorithm.initialize()
        }
        await(fut1, 0.0, 0.5)
        val fut2 = algorithm.global.cursor.step { implicit tx =>
          algorithm.evaluateAndUpdate()
        }
        await(fut2, 0.5, 0.5)
        algorithm
      }
    }
  }

  def DurableInit(config: Algorithm.Config, init: Boolean): Processor[Algorithm.Durable] = {
    val res = new DurableInit(config, init = init)
    import Algorithm.executionContext
    res.start()
    res
  }

  private final class DurableInit(config: Algorithm.Config, init: Boolean)
    extends ProcessorImpl[Algorithm.Durable, Processor[Algorithm.Durable]] with Processor[Algorithm.Durable] {

    def body(): Algorithm.Durable = {
      val algorithm = Algorithm.durable(config)
      val cursor = algorithm.global.cursor

      val isNew = cursor.step { implicit tx =>
        algorithm.genome.chromosomes().isEmpty
      }
      if (isNew && init) {
        val futInit = cursor.step { implicit tx => algorithm.initialize() }
        await(futInit, 0, 0.5)
      }
      if (isNew && init) {
        val fut0 = cursor.step { implicit tx =>
          algorithm.evaluateAndUpdate()
        }
        await(fut0, 0.5, 0.5)
      }
      algorithm
    }
  }

  final class DurableApp(config: Algorithm.Config) extends GenApp[Durable] {
    type S = Durable
    type A = Algorithm.Durable

    def init(): Processor[A] = DurableInit(config, init = true)
  }

  private final class DurableHybridInit(config: Algorithm.Config, init: Boolean)
    extends ProcessorImpl[Algorithm.InMemory, Processor[Algorithm.InMemory]] with Processor[Algorithm.InMemory] {

    def body(): Algorithm.InMemory = {
      val algorithm = Algorithm.durableHybrid(config)
      val cursor = algorithm.global.cursor

      val isNew = cursor.step { implicit tx =>
        algorithm.genome.chromosomes().isEmpty
      }
      if (isNew && init) {
        val futInit = cursor.step { implicit tx => algorithm.initialize() }
        await(futInit, 0, 0.5)
      }
      if (isNew && init) {
        val fut0 = cursor.step { implicit tx =>
          algorithm.evaluateAndUpdate()
        }
        await(fut0, 0.5, 0.5)
      }
      algorithm
    }
  }

  /** Durable/In-Memory hybrid for speed increase. */
  final class DurableHybridApp(config: Algorithm.Config) extends GenApp[InMemory] {
    type S = InMemory
    type A = Algorithm.InMemory

    def init(): Processor[A] = {
      import Algorithm.executionContext
      val res = new DurableHybridInit(config, init = true)
      res.start()
      res
    }
  }

  final class ConfluentApp(config: Algorithm.Config) extends GenApp[confluent.Confluent] {
    type S = confluent.Confluent
    type A = Algorithm.Confluent

    //    def getIteration(a: A, iterInc: Int): Int = {
    //      val csr = a.global.cursor
    //      val inp = csr.step { implicit tx =>
    //        implicit val dtx = tx.durable
    //        csr.position
    //      }
    //      inp.size / 2
    //    }

    def init(): Processor[A] = {
      import Algorithm.executionContext
      val res = new Init
      res.start()
      res
    }

    private final class Init extends ProcessorImpl[A, Processor[A]] with Processor[A] {
      protected def body(): A = {
        import config._
        val dir = databaseFile // file("database"  ) / (if (args.length > 0) args(0) else "betanovuss")
        val in  = audioFile // file("audio_work") / (if (args.length > 1) args(1) else "Betanovuss150410_1Cut.aif")
        val algorithm = blocking(Algorithm.confluent(config)) // , dir = dir, input = in))

        val cursor = algorithm.global.cursor
        val isNew = cursor.step { implicit tx =>
          val _isNew = algorithm.genome.chromosomes().isEmpty
          if (_isNew) {
            val futInit = algorithm.initialize()
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
  // ---- abstract ----

  type A <: Algorithm[S]

  def init(): Processor[A]

  // ---- impl ----

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

  var algorithm = Option.empty[A]
  var busy      = Option.empty[Processor[Any]]

  import Algorithm.executionContext

  val ggRemoveUGen = Button("Remove UGen…") {
    if (busy.isEmpty) algorithm.foreach { algo: Algorithm[S] =>
      OptionPane.textInput(message = "UGen Name:", initial = "GbmanL").show(None).foreach { ugenName =>
        val proc = Processor[Int](s"Remove $ugenName") { self =>
          val cursor = algo.global.cursor
          val cs = cursor.step { implicit tx =>
            algo.genome.chromosomes()
          }
          val numChromo = cs.size
          val count = (0 /: cs.zipWithIndex) { case (countIn, (c, ci)) =>
            val countOut = cursor.step { implicit tx =>
              val these = c.vertices.iterator.collect {
                case uv: Vertex.UGen[S] if uv.boxName == ugenName => uv
              } .toIndexedSeq
              these.foreach(impl.MutationImpl.removeVertex2[S](algo.config, c, _)(tx, algo.global.rng))
              countIn + these.size
            }
            self.progress = (ci + 1).toDouble / numChromo
            self.checkAborted()
            countOut
          }

          count
        }

        setBusy(proc)
        proc.onSuccess {
          case count => println(s"Removed $count $ugenName UGens.")
        }
      }
    }
  }

  val ggValidate = Button("Validate") {
    algorithm.foreach { algo: Algorithm[S] =>
      algo.global.cursor.step { implicit tx =>
        val ok = algo.genome.chromosomes().forall { c =>
          c.validate1()
        }
        if (ok) {
          println(s"No errors have been found.")
        }
      }
    }
  }

  val pTop = new FlowPanel(
    new Label("Best:"  ), ggBest,
    new Label("Avg:"   ), ggAvg,
    new Label("Median:"), ggMedian,
    new Label("Iter:"  ), ggIter,
    ggRemoveUGen,
    ggValidate
  )

  def updateStats(a: A): Unit = {
    val csr = a.global.cursor
    val (fit: Vec[Float], numIter: Int) = csr.step { implicit tx => a.genome.fitness() -> a.global.numIterations() }
    import kollflitz.Ops._
    val sorted = fit.sortedT
    ggBest  .text = f"${sorted.last  }%1.3f"
    ggAvg   .text = f"${sorted.mean  }%1.3f"
    ggMedian.text = f"${sorted.median}%1.3f"
    ggIter  .text = numIter.toString
  }

  val ggProgress = new ProgressBar

  val ggAbort = Button("X") {
    busy.foreach(_.abort())
  }
  ggAbort.enabled = false

  def setBusy(fut: Processor[Any]): Unit = {
    busy = Some(fut)
    ggProgress.value      = 0
    ggProgress.background = Color.yellow
    ggAbort.enabled       = true

    fut.addListener {
      case p @ Processor.Progress(_, _) => defer { ggProgress.value = p.toInt }
    }

    fut.onComplete {
      case _ => defer {
        busy = None
        ggAbort.enabled = false
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

  def stepN(num0: Int): Unit =
    if (busy.isEmpty) {
      val num = math.max(1, num0)
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

  val ggStep1 = Button("Step 1") {
    stepN(1)
  }
  val ggStepNum = new TextField(3) {
    text = 100.toString
  }
  val ggStepN = Button("Step N") {
    stepN(ggStepNum.text.toInt)
  }

  private[this] final class StepN(num: Int, a: A)
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = {
      val weight = 1.0 / num
      for (i <- 0 until num) {
        // println(s"-------------STEP $i-------------")
        val futI = a.iterate()
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
      try { algorithm.foreach(_.close()) } catch { case NonFatal(ex) => ex.printStackTrace() }
      sys.exit()
    }
  }
}