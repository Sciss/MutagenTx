/*
 *  CopyingAlgorithm.scala
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

import java.util

import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.{data, stm}
import de.sciss.processor.Processor
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor

object CopyingAlgorithm {
  def apply[S <: Sys[S]](system: S, input: File, global: GlobalState[S], genomeH: stm.Source[S#Tx, Genome[S]])
                        (implicit cursor: stm.Cursor[S], ord: data.Ordering[S#Tx, Vertex[S]]): Algorithm[S] = {
    val futInput = TxnExecutor.defaultAtomic { implicit tx =>
      impl.EvaluationImpl.getInputSpec(input)
    }
    val (_inputExtr, _inputSpec) = Await.result(futInput, Duration.Inf)

    new Impl(system, genomeH, global, input = input, inputExtr = _inputExtr, inputSpec = _inputSpec)
  }

  private final class Impl[S <: Sys[S]](val system: S, handle: stm.Source[S#Tx, Genome[S]],
                                        val global: GlobalState[S],
                                        val input: File, val inputExtr: File, val inputSpec: AudioFileSpec)
                                       (implicit val ord: data.Ordering[S#Tx, Vertex[S]])
    extends AlgorithmImpl[S] { algo =>

    override def toString = s"CopyingAlgorithm(input = $input)@${hashCode().toHexString}"

    def genome(implicit tx: S#Tx): Genome[S] = handle()

    type Global = GlobalState[S]

    import global.rng

    /** Produces a sequence of `n` items by crossing each two parents from the input `sel` selection. */
    def crossover(sq: Vec[stm.Source[S#Tx, C]], n: Int): Vec[stm.Source[S#Tx, C]] = {
      var res = Vector.empty[stm.Source[S#Tx, C]]
      while (res.size < n) {
        val idx0      = res.size << 1
        val chosen0H  = sq( idx0      % sq.size)
        val chosen1H  = sq((idx0 + 1) % sq.size)

        val hs = global.cursor.step { implicit tx =>
          val chosen1   = chosen0H()
          val chosen2   = chosen1H()
          val chosen1c  = mkCopy(chosen1)
          val chosen2c  = mkCopy(chosen2)
          impl.CrossoverImpl(algo, chosen1 = chosen1c, chosen2 = chosen2c)

          implicit val cSer = chromosomeSerializer[S]
          val _res0 = tx.newHandle(chosen1c)
          val _res1 = if (res.size + 1 == n) _res0 :: Nil else {
            _res0 :: tx.newHandle(chosen2c) :: Nil
          }
          _res1
        }
        hs.foreach { h => res :+= h }
      }
      res
    }

    def mkCopy(in: C)(implicit tx: S#Tx): C = {
      val map = new util.IdentityHashMap[Vertex[S], Vertex[S]]()
      val top = Topology.empty[S, Vertex[S], Edge[S]]
      in.vertices.iterator.foreach { vIn =>
        val vOut = vIn.copy()
        map.put(vIn, vOut)
        top.addVertex(vOut)
      }
      in.edges.iterator.foreach {
        case Edge(srcIn, tgtIn, inlet) =>
          val srcOut  = map.get(srcIn).asInstanceOf[Vertex.UGen[S]]
          val tgtOut  = map.get(tgtIn)
          val eOut = Edge(sourceVertex = srcOut, targetVertex = tgtOut, inletIndex = inlet)
          top.addEdge(eOut)
      }
      top
    }

    /** Produces a sequence of `n` items by mutating the input `sel` selection. */
    def mutate(sq: Vec[stm.Source[S#Tx, C]], n: Int): Vec[stm.Source[S#Tx, C]] = {
      var res = Vector.empty[stm.Source[S#Tx, C]]

      while (res.size < n) {
        val chosenH = sq(res.size % sq.size)
        val hOpt: Option[stm.Source[S#Tx, C]] = global.cursor.step { implicit tx =>
          val chosen0 = chosenH()
          val chosen1 = mkCopy(chosen0)
          val ok = impl.MutationImpl(algo, chosen = chosen1)
          if (ok) {
            implicit val cSer = chromosomeSerializer[S]
            Some(tx.newHandle(chosen1))
          } else None
        }
        hOpt.foreach { h => res :+= h }
      }
      res
    }

    def iterate(): Processor[Unit] = {
      val (el, sel, pop) = global.cursor.step { implicit tx =>
        // if (DEBUG) println(s"iterate - inputAccess ${tx.inputAccess}")
        val cs    = genome.chromosomes()
        val fit   = genome.fitness()
        val all   = cs zip fit
        val _el   = elitism(all) // .toSet
        val _sel  = select (all) .map(tx.newHandle(_))
        (_el, Util.scramble(_sel.toIndexedSeq), all.size)
      }

      val nGen    = pop - el.size
      val nMut    = (Algorithm.mutationProb * nGen + 0.5).toInt
      val nCross  = nGen - nMut

      // if (DEBUG) println(s"pop $pop, el ${el.size}, sel ${sel.size}, nMut $nMut, nCross $nCross")

      val mut     = mutate    (sel, nMut  )
      val cross   = crossover (sel, nCross)

      global.cursor.step { implicit tx =>
        genome.chromosomes() = el ++ (mut ++ cross).map(_.apply())
        evaluateAndUpdate()
      }
    }
  }
}
