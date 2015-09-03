/*
 *  ConfluentAlgorithm.scala
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
package impl

import de.sciss.file._
import de.sciss.lucre.stm.{Sys, DataStore}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{confluent, data, stm}
import de.sciss.processor.Processor
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor

object ConfluentAlgorithm {
  type S = confluent.Confluent

  def tmp(config: Algorithm.Config, input: File): Algorithm.Confluent = {
    val cfg = BerkeleyDB.Config()
    // cfg.lockTimeout = Duration(2000, TimeUnit.MILLISECONDS)
    val dbf = BerkeleyDB.tmp(cfg)
    create(config, dbf, input)
  }

  def apply(config: Algorithm.Config, dir: File, input: File): Algorithm.Confluent = {
    val dbf = BerkeleyDB.factory(dir)
    create(config, dbf, input)
  }

  private def create(config: Algorithm.Config, dbf: DataStore.Factory, _input: File): Algorithm.Confluent = {
    val futInput = TxnExecutor.defaultAtomic { implicit tx =>
      impl.EvaluationImpl.getInputSpec(config, _input)
    }
    val (_inputExtr, _inputSpec) = Await.result(futInput, Duration.Inf)

    implicit val system = confluent.Confluent(dbf)
    implicit val ord: data.Ordering[S#Tx, Vertex[S]] = ConfluentOrdering[Vertex[S]]
    implicit val genomeSer = Genome.serializer[S]
    // implicit val globalSer = GlobalState.Confluent.serializer
    val (handle: stm.Source[S#Tx, Genome[S]], global: GlobalState.Confluent) = system.rootWithDurable { implicit tx =>
      implicit val dtx = system.durableTx(tx)
      Genome.empty[S]
    } { implicit tx =>
      GlobalState.Confluent()
    }

    new Impl(config, system, handle, global, input = _input, inputExtr = _inputExtr, inputSpec = _inputSpec)
  }

  private final class Impl(val config: Algorithm.Config, system: S, handle: stm.Source[S#Tx, Genome[S]],
                           val global: GlobalState.Confluent,
                           val input: File, val inputExtr: File, val inputSpec: AudioFileSpec)
    extends AlgorithmImpl[S] /* with Algorithm.Confluent */ { algo =>

    import config._

    type Global = GlobalState.Confluent

    override def toString = s"ConfluentAlgorithm(input = $input)@${hashCode().toHexString}"

    implicit val ord: data.Ordering[S#Tx, Vertex[S]] = ConfluentOrdering[Vertex[S]]

    def genome(implicit tx: S#Tx): Genome[S] = handle()

    def ephemeral = false

    def close(): Unit = system.close()

    import global.rng

    /** Produces a sequence of `n` items by crossing each two parents from the input `sel` selection.
      *
      * It assumes the invoking transaction is 'up-to-date' and will cause
      * the selection's cursors to step from this transaction's input access.
      */
    def crossover(sq: Vec[stm.Source[S#Tx, C]], n: Int,
                          inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, C])] = {
      var res = Vector.empty[(S#Acc, confluent.Source[S, C])]
      while (res.size < n) {
        val idx0      = res.size << 1
        val chosen0H  = sq( idx0      % sq.size)
        val chosen1H  = sq((idx0 + 1) % sq.size)

        val csr       = algo.global.forkCursor
        val hs = csr.stepFrom(inputAccess) { implicit tx =>
          val chosen1 = chosen0H()
          val chosen2 = chosen1H()
          impl.CrossoverImpl(algo, chosen1 = chosen1, chosen2 = chosen2)

          implicit val cSer = Chromosome.serializer[S]
          val _res0 = tx.newHandleM(chosen1)
          val _res1 = if (res.size + 1 == n) _res0 :: Nil else {
            _res0 :: tx.newHandleM(chosen2) :: Nil
          }

          _res1
          // Vector(c1, c2)
        }

        val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
        if (Algorithm.DEBUG) println(s"$hs - $pos")
        hs.foreach(h => res :+= (pos, h))
      }
      res
    }

    /** Produces a sequence of `n` items by mutating the input `sel` selection.
      *
      * It assumes the invoking transaction is 'up-to-date' and will cause
      * the selection's cursors to step from this transaction's input access.
      */
    def mutate(sq: Vec[stm.Source[S#Tx, C]], n: Int,
                       inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, C])] = {
      var res = Vector.empty[(S#Acc, confluent.Source[S, C])]

      while (res.size < n) {
        val chosenH = sq(res.size % sq.size)
        val csr = algo.global.forkCursor
        val hOpt = csr.stepFrom(inputAccess) { implicit tx =>
          val chosen0 = chosenH()
          val ok = impl.MutationImpl(algo, chosen = chosen0)
          if (ok) {
            implicit val cSer = Chromosome.serializer[S]
            Some(tx.newHandleM(chosen0))
          } else None
        }
        hOpt.foreach { h =>
          val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
          if (Algorithm.DEBUG) println(s"$h - $pos")
          res :+=(pos, h)
        }
      }
      res
    }

    def iterate(): Processor[Unit] = {
      val (el, sel, pop, inputAccess) = global.cursor.step { implicit tx =>
        // if (DEBUG) println(s"iterate - inputAccess ${tx.inputAccess}")
        val cs    = genome.chromosomes()
        val fit   = genome.fitness()
        val all   = cs zip fit
        val _el   = elitism(all) // .toSet
        val _sel  = select (all) .map(tx.newHandle(_))
        (_el, Util.scramble(_sel.toIndexedSeq), all.size, tx.inputAccess)
      }

      val nGen    = pop - el.size
      val nMut    = (mutationProb * nGen + 0.5).toInt
      val nCross  = nGen - nMut

      // if (DEBUG) println(s"pop $pop, el ${el.size}, sel ${sel.size}, nMut $nMut, nCross $nCross")

      val mut     = mutate    (sel, nMut  , inputAccess)
      val cross   = crossover (sel, nCross, inputAccess)

      global.cursor.step { implicit tx =>
        genome.chromosomes() = el ++ (mut ++ cross).map { case (access, h) =>
          h.meld(access)
        }
        evaluateAndUpdate()
      }
    }
  }
}
