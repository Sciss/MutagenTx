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

import java.util.concurrent.TimeUnit

import de.sciss.file._
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{data, event => evt, stm}
import de.sciss.processor.Processor
import de.sciss.serial.DataOutput
import de.sciss.synth.UGenSpec
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.language.higherKinds

object Algorithm {
  val DEBUG = false

  // ---- generation ----
  val population      : Int     = 1000
  val constProb       : Double  = 0.5
  val minNumVertices  : Int     = 30
  val maxNumVertices  : Int     = 256 // 100
  val nonDefaultProb  : Double  = 0.99 // 0.5

  // ---- evaluation ----
  val numCoeffs       : Int     = 42
  val normalizeCoeffs : Boolean = false // true
  val maxBoost        : Double  = 10.0
  val temporalWeight  : Double  = 0.3
  val vertexPenalty   : Double  = 0.01

  // ---- breeding ----
  val selectionFrac   : Double  = 0.33
  val numElitism      : Int     = 5
  val mutMin          : Int     = 2
  val mutMax          : Int     = 4
  val mutationProb    : Double  = 0.75

  implicit val executionContext: ExecutionContext = {
    ExecutionContext.Implicits.global
    // SoundProcesses.executionContext
    // val ex = Executors.newFixedThreadPool(6)
    // ExecutionContext.fromExecutor(ex)
  }

  def tmpConfluent(input: File): Confluent =
    impl.ConfluentAlgorithm.tmp(input)

  def confluent(dir: File, input: File): Confluent =
    impl.ConfluentAlgorithm.apply(dir = dir, input = input)

  def durable(dir: File, input: File): Durable = {
    type S = evt.Durable
    val dbc = BerkeleyDB.Config()
    dbc.lockTimeout = Duration(0, TimeUnit.SECONDS)
    val dbf = BerkeleyDB.factory(dir, dbc)
    implicit val system = evt.Durable(dbf)

    implicit object VertexOrd extends data.Ordering[S#Tx, Vertex[S]] {
      def compare(a: Vertex[S], b: Vertex[S])(implicit tx: S#Tx): Int = {
        val aid = stm.Escape.durableID(a.id)
        val bid = stm.Escape.durableID(b.id)
        if (aid < bid) -1 else if (aid > bid) 1 else 0
      }
    }

    val rootH = system.root[(GlobalState.Durable, Genome[S])] { implicit tx =>
      (GlobalState.Durable(), Genome.empty[S])
    }
    // Yes, I know... Not nice...
    val (global, genomeH) = system.step { implicit tx =>
      val (_global, _genome) = rootH()
      (_global, tx.newHandle(_genome))
    }

    //    val (global: GlobalState.Durable, genomeH: stm.Source[S#Tx, Genome[S]]) = system.step { implicit tx =>
    //      (GlobalState.Durable(), tx.newHandle(Genome.empty[S]))
    //    }

    lazy val cleaner = { (_tx: S#Tx, elite: Vec[Chromosome[S]]) =>
      implicit val tx = _tx
      val g   = a.genome
      val old = g.chromosomes()
      val fit = g.fitness()
      g.chromosomes() = Vector.empty
      val eliteSet = elite.toSet
      // val ser   = SynthGraphs.ValueSerializer //implicitly[ImmutableSerializer[SynthGraph]]
      val iter  = a.global.numIterations()
      val store = iter % 10 == 0
      val f     = dir.parent / s"${dir.name}_iter$iter.bin"
      lazy val out = DataOutput.open(f)

      (old zip fit).foreach { case (c, fit0) =>
        if (!eliteSet.contains(c)) {
          if (store && fit0 > 0.4f) {
            val graph = impl.ChromosomeImpl.mkSynthGraph(c, mono = true, removeNaNs = false, config = true)
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

    lazy val a: Algorithm.Durable = impl.CopyingAlgorithm[S, GlobalState.Durable](system = system, input = input,
      global = global, genomeH = genomeH, ephemeral = true, cleaner = Some(cleaner))
    a
  }

  def inMemory(input: File): Algorithm[evt.InMemory] = {
    type S = evt.InMemory
    implicit val system = evt.InMemory()

    implicit object VertexOrd extends data.Ordering[S#Tx, Vertex[S]] {
      def compare(a: Vertex[S], b: Vertex[S])(implicit tx: S#Tx): Int = {
        val aid = stm.Escape.inMemoryID(a.id)
        val bid = stm.Escape.inMemoryID(b.id)
        if (aid < bid) -1 else if (aid > bid) 1 else 0
      }
    }

    val (global: GlobalState[S], genomeH: stm.Source[S#Tx, Genome[S]]) = system.step { implicit tx =>
      (GlobalState.InMemory(), tx.newHandle(Genome.empty[S]))
    }

    impl.CopyingAlgorithm[S, GlobalState[S]](system = system, input = input, global = global,
      genomeH = genomeH, ephemeral = true)
  }

  type InMemory = Algorithm[evt.InMemory] {
    type Global = GlobalState[evt.InMemory]
  }

  type Durable = Algorithm[evt.Durable] {
    type Global = GlobalState.Durable
  }

  type Confluent = Algorithm[ConfluentReactive] {
    type Global = GlobalState.Confluent
  }
}
trait Algorithm[S <: Sys[S]] {
  type C = Chromosome[S]

  def genome(implicit tx: S#Tx): Genome[S]

  /** Input target sound file. */
  def input: File

  /** Target sound's feature extraction file. */
  def inputExtr: File

  /** Target sound's specification. */
  def inputSpec: AudioFileSpec

  def system: S

  type Global <: GlobalState[S]

  val global: Global

  implicit def ord: data.Ordering[S#Tx, Vertex[S]]

  /** Creates the initial population of size `n`. */
  def initialize(n: Int)(implicit tx: S#Tx): Processor[Unit]

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
}