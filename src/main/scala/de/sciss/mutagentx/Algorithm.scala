/*
 *  Algorithm.scala
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

import java.io.Closeable
import java.util.concurrent.TimeUnit

import de.sciss.file._
import de.sciss.lucre
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{data, stm}
import de.sciss.processor.Processor
import de.sciss.serial.{Serializer, DataOutput}
import de.sciss.synth.UGenSpec
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.language.higherKinds

object Algorithm {
  val DEBUG = false

  object SysType {
    case object InMemory  extends SysType
    case object Durable   extends SysType
    case object Hybrid    extends SysType
    case object Confluent extends SysType
  }
  sealed trait SysType

  // N.B. sample rate is taken from audioFile!
  case class Config(
    // ---- files and type ----
    audioFile       : File    = file("target.aif"),
    databaseFile    : File    = file("output.db"),
    tpe             : SysType = SysType.Hybrid,
    // ---- generation ----
    population      : Int     = 500,
    constProb       : Double  = 0.5,
    minNumVertices  : Int     = 64,
    maxNumVertices  : Int     = 256,
    nonDefaultProb  : Double  = 0.95,
    allowedUGens    : Set[String] = Set.empty,
    // ---- evaluation ----
    numMFCC         : Int     = 42,
    normalizeMFCC   : Boolean = false,
    maxBoost        : Double  = 10.0,
    temporalWeight  : Double  = 0.3,
    // ---- graph penalty ----
    vertexPenalty   : Double  = 0.02,
    graphPenaltyIter: Int     = 10,
    graphPenaltyCeil: Double  = 0.275,
    graphPenaltyAmt : Double  = 0.2,
    graphPenaltyCoin: Double  = 0.25,
    // ---- breeding ----
    selectionFrac   : Double  = 0.33,
    numElitism      : Int     = 3,
    mutMin          : Int     = 2,
    mutMax          : Int     = 4,
    mutationProb    : Double  = 0.75,
    numGolem        : Int     = 15
  )

  implicit val executionContext: ExecutionContext = {
    ExecutionContext.Implicits.global
    // SoundProcesses.executionContext
    // val ex = Executors.newFixedThreadPool(6)
    // ExecutionContext.fromExecutor(ex)
  }

  def tmpConfluent(config: Algorithm.Config): Confluent =
    impl.ConfluentAlgorithm.tmp(config, ???)

  def confluent(config: Algorithm.Config): Confluent =
    impl.ConfluentAlgorithm.apply(config, dir = ???, input = ???)

  implicit object InMemoryVertexOrdering extends data.Ordering[stm.InMemory#Tx, Vertex[stm.InMemory]] {
    type S = stm.InMemory

    def compare(a: Vertex[S], b: Vertex[S])(implicit tx: S#Tx): Int = {
      val aid = stm.Escape.inMemoryID(a.id)
      val bid = stm.Escape.inMemoryID(b.id)
      if (aid < bid) -1 else if (aid > bid) 1 else 0
    }
  }

  implicit object DurableVertexOrdering extends data.Ordering[stm.Durable#Tx, Vertex[stm.Durable]] {
    type S = stm.Durable

    def compare(a: Vertex[S], b: Vertex[S])(implicit tx: S#Tx): Int = {
      val aid = stm.Escape.durableID(a.id)
      val bid = stm.Escape.durableID(b.id)
      if (aid < bid) -1 else if (aid > bid) 1 else 0
    }
  }

  def mkCleaner[S <: Sys[S]](a: => Algorithm[S], dir: File): (S#Tx, Vec[Chromosome[S]]) => Unit = { (_tx, elite) =>
    implicit val tx = _tx
    val g   = a.genome
    val old = g.chromosomes()
    val fit = g.fitness()
    g.chromosomes() = Vector.empty
    val eliteSet: Set[Chromosome[S]] = elite.toSet
    // val ser   = SynthGraphs.ValueSerializer //implicitly[ImmutableSerializer[SynthGraph]]
    val iter  = a.global.numIterations()
    val store = iter % 5 /* 10 */ == 0
    val f     = dir.parent / s"${dir.name}_iter$iter.bin"
    lazy val out = DataOutput.open(f)

    val thresh = if (fit.isEmpty) 0.4f else math.min(0.4f, fit.sorted.apply(fit.size/2))

    (old zip fit).foreach { case (c, fit0) =>
      if (!eliteSet.contains(c)) {
        if (store && fit0 > thresh /* 0.4f */) {
          val graph = MkSynthGraph(c, mono = true, removeNaNs = false, config = true, ranges = false)
          val input = SOMGenerator.Input(graph, iter = iter, fitness = fit0)
          SOMGenerator.Input.serializer.write(input, out)
        }
        val v = c.vertices.iterator.toIndexedSeq
        c.dispose()
        v.foreach(_.dispose())
      }
    }

    if (store) out.close()
    ()
  }

  def durable(config: Algorithm.Config): Durable = {
    import config.{databaseFile => dir, audioFile => input}
    type S = stm.Durable
    val dbc = BerkeleyDB.Config()
    dbc.lockTimeout = Duration(0, TimeUnit.SECONDS)
    val dbf = BerkeleyDB.factory(dir, dbc)
    implicit val system: S = stm.Durable(dbf)

    val rootH = system.root[(GlobalState.Durable, Genome[S])] { implicit tx =>
      (GlobalState.Durable(), Genome.empty[S])
    }
    // Yes, I know... Not nice...
    val (global, genomeH) = system.step { implicit tx =>
      val (_global, _genome) = rootH()
      (_global, tx.newHandle(_genome))
    }

    lazy val cleaner = mkCleaner(a, dir)

    lazy val a: Algorithm.Durable = impl.CopyingAlgorithm[S, GlobalState.Durable](config,
      system = system, input = input,
      global = global, genomeH = genomeH, ephemeral = true, cleaner = Some(cleaner))
    a
  }

  def durableHybrid(config: Algorithm.Config): InMemory = {
    import config.{databaseFile => dir, audioFile => input}
    type S = stm.InMemory
    type D = stm.Durable

    val dbc = BerkeleyDB.Config()
    dbc.lockTimeout = Duration(0, TimeUnit.SECONDS)
    val dbf = BerkeleyDB.factory(dir, dbc)
    val systemD: D = stm.Durable(dbf)
    // implicit val system: S = stm.Durable(dbf)

    implicit val genomeSer  = Genome.serializer[D]
    implicit val tupSer     = Serializer.tuple2[D#Tx, D#Acc, GlobalState.Durable, Genome[D]]
    val rootH = systemD.root[(GlobalState.Durable, Genome[D])] { implicit dtx =>
      implicit val tx = dtx.inMemory
      (GlobalState.Durable(), Genome.empty[D])
    }
    // Yes, I know... Not nice...
    val (global, genomeH) = systemD.step { implicit tx =>
      val (_globalD, _genomeD) = rootH()
      val _global = GlobalState.DurableHybrid(_globalD)
      val _genome: Genome[S] = Genome.DurableHybrid(config, _global, _genomeD)
      // _globalD.numIterations() = _iter0
      val itx = tx.inMemory
      (_global, itx.newHandle(_genome))
    }

    // lazy val cleaner = mkCleaner(a, dir)

    lazy val a: Algorithm.InMemory = impl.CopyingAlgorithm[S, GlobalState.InMemory](config,
      system = systemD,
      input = input, global = global, genomeH = genomeH, ephemeral = true, cleaner = None /* Some(cleaner) */)
    a
  }

  def inMemory(config: Algorithm.Config): Algorithm[stm.InMemory] = {
    import config.{audioFile => input}
    type S = stm.InMemory
    implicit val system = stm.InMemory()

    val (global: GlobalState[S], genomeH: stm.Source[S#Tx, Genome[S]]) = system.step { implicit tx =>
      (GlobalState.InMemory(), tx.newHandle(Genome.empty[S]))
    }

    impl.CopyingAlgorithm[S, GlobalState[S]](config, system = system, input = input, global = global,
      genomeH = genomeH, ephemeral = true)
  }

  type InMemory = Algorithm[stm.InMemory] {
    type Global = GlobalState[stm.InMemory]
  }

  type Durable = Algorithm[stm.Durable] {
    type Global = GlobalState.Durable
  }

  type Confluent = Algorithm[lucre.confluent.Confluent] {
    type Global = GlobalState.Confluent
  }
}
trait Algorithm[S <: Sys[S]] extends Closeable {
  type C = Chromosome[S]

  def genome(implicit tx: S#Tx): Genome[S]

  /** Input target sound file. */
  def input: File

  /** Target sound's feature extraction file. */
  def inputExtr: File

  /** Target sound's specification. */
  def inputSpec: AudioFileSpec

  // def system: S

  type Global <: GlobalState[S]

  val global: Global

  implicit def ord: data.Ordering[S#Tx, Vertex[S]]

  /** Creates the initial population of size `n`. */
  def initialize()(implicit tx: S#Tx): Processor[Unit]

  /** Creates an individual chromosome. */
  def mkIndividual()(implicit tx: S#Tx): C

  /** Adds a random vertex to an existing chromosome. */
  def addVertex(c: C)(implicit tx: S#Tx): Unit

  /** Utility method that collects the general arguments into which other `GE` elements may be plugged. */
  def geArgs(spec: UGenSpec): Vec[UGenSpec.Argument]

  /** Adds vertices a vertex `v` newly added to a chromosome `c` to ensure
    * it is fully wired.
    */
  def completeUGenInputs(c: C, v: Vertex.UGen[S])(implicit tx: S#Tx): Unit

  /** Creates a random constant vertex. */
  def mkConstant()(implicit tx: S#Tx): Vertex.Constant[S]

  /** Creates a random UGen vertex. */
  def mkUGen()(implicit tx: S#Tx): Vertex.UGen[S]

  /** Runs the evaluation on all chromosomes of the current genome,
    * returning the fitness vector (the genome is not updated).
    */
  def evaluate()(implicit tx: S#Tx): Processor[Vec[Float]]

  /** Runs the evaluation on all chromosomes of the current genome,
    * updating the genome's fitness vector.
    */
  def evaluateAndUpdate()(implicit tx: S#Tx): Processor[Unit]

  /** Runs the selection stage of the algorithm, using `all` inputs which
    * are chromosomes paired with their fitness values.
    */
  def select(all: Vec[(C, Float)])(implicit tx: S#Tx): Set[C]

  /** Selects the best matching chromosomes. */
  def elitism(all: Vec[(C, Float)])(implicit tx: S#Tx): Vec[C]

  /** Performs one iteration of the algorithm, assuming that current population
    * was already evaluated. Steps:
    *
    * - elitism
    * - selection
    * - breeding: mutation and cross-over
    * - evaluation
    */
  def iterate(): Processor[Unit]

  val config: Algorithm.Config
}