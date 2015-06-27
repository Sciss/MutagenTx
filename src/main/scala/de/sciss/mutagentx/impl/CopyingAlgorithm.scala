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

import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.{data, stm}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.UGenSpec
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.{Await, blocking}
import scala.concurrent.duration.Duration
import scala.concurrent.stm.TxnExecutor

object CopyingAlgorithm {
  def apply[S <: Sys[S], G <: GlobalState[S]](system: S, input: File, global: G, genomeH: stm.Source[S#Tx, Genome[S]],
                                              ephemeral: Boolean, cleaner: Option[(S#Tx, Vec[Chromosome[S]]) => Unit] = None)
                        (implicit cursor: stm.Cursor[S], ord: data.Ordering[S#Tx, Vertex[S]]): Algorithm[S] { type Global = G } = {
    val futInput = TxnExecutor.defaultAtomic { implicit tx =>
      impl.EvaluationImpl.getInputSpec(input)
    }
    val (_inputExtr, _inputSpec) = Await.result(futInput, Duration.Inf)

    new Impl[S, G](system, genomeH, global, input = input, inputExtr = _inputExtr, inputSpec = _inputSpec,
      ephemeral = ephemeral, cleaner = cleaner)
  }

  def mkCopy[S <: Sys[S]](in: Chromosome[S])(implicit tx: S#Tx, ord: data.Ordering[S#Tx, Vertex[S]]): Chromosome[S] = {
    var map = Map.empty[Vertex[S], Vertex[S]]
    val top = Topology.empty[S, Vertex[S], Edge[S]]
    in.vertices.iterator.foreach { vIn =>
      val vOut = vIn.copy()
      map += vIn -> vOut
      top.addVertex(vOut)
    }
    in.edges.iterator.foreach {
      case Edge(srcIn, tgtIn, inlet) =>
        val srcOut = map.get(srcIn) match {
          case Some(vu: Vertex.UGen[S]) => vu
          case _ => throw new NoSuchElementException
        }
        val tgtOut = map.getOrElse(tgtIn, throw new NoSuchElementException)
        val eOut = Edge(sourceVertex = srcOut, targetVertex = tgtOut, inletIndex = inlet)
        top.addEdge(eOut)
    }
    top
  }

  private sealed trait VertexC
  private final class VertexCU(val info: UGenSpec) extends VertexC
  private final class VertexCC(val f: Float) extends VertexC

  def mkCopyT[S <: Sys[S], T <: Sys[T]](in: Chromosome[S], cs: stm.Cursor[S], ct: stm.Cursor[T])
                                       (implicit ord: data.Ordering[T#Tx, Vertex[T]]): Chromosome[T] = {
    var map  = Map.empty[Vertex[S], VertexC]
    var sq   = Vec.empty[VertexC]
    var mapT = Map.empty[VertexC, Vertex[T]]
    val top = ct.step { implicit tx => Topology.empty[T, Vertex[T], Edge[T]] }

    cs.step { implicit tx =>
      in.vertices.iterator.foreach { vIn =>
        val vOut = vIn match {
          case vu: Vertex.UGen[S]     => new VertexCU(vu.info)
          case vc: Vertex.Constant[S] => new VertexCC(vc.f())
        }
        map += vIn -> vOut
        sq :+= vOut
      }
    }
    ct.step { implicit tx =>
      sq.foreach { vIn =>
        val vOut = vIn match {
          case vu: VertexCU => Vertex.UGen[T](vu.info)
          case vc: VertexCC => Vertex.Constant[T](vc.f)
        }
        mapT += vIn -> vOut
        top.addVertex(vOut)
      }
    }

    val edges = cs.step { implicit tx => in.edges.iterator.toIndexedSeq }

    ct.step { implicit tx =>
      edges.foreach {
        case Edge(srcIn, tgtIn, inlet) =>
          val srcOut = map.get(srcIn).flatMap(mapT.get) match {
            case Some(vu: Vertex.UGen[T]) => vu
            case _ => throw new NoSuchElementException
          }
          val tgtOut: Vertex[T] = map.get(tgtIn).flatMap(mapT.get).getOrElse(throw new NoSuchElementException)
          val eOut = Edge(sourceVertex = srcOut, targetVertex = tgtOut, inletIndex = inlet)
          top.addEdge(eOut)
      }
    }
    top
  }

  private final class Impl[S <: Sys[S], G <: GlobalState[S]](val system: S, handle: stm.Source[S#Tx, Genome[S]],
                                        val global: G,
                                        val input: File, val inputExtr: File, val inputSpec: AudioFileSpec,
                                        val ephemeral: Boolean,
                                        cleaner: Option[(S#Tx, Vec[Chromosome[S]]) => Unit])
                                       (implicit val ord: data.Ordering[S#Tx, Vertex[S]])
    extends AlgorithmImpl[S] { algo =>

    override def toString = s"CopyingAlgorithm(input = $input)@${hashCode().toHexString}"

    def genome(implicit tx: S#Tx): Genome[S] = handle()

    type Global = G

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
      import Algorithm.executionContext
      val res = new IterImpl
      res.start()
      res
    }

    private final class IterImpl extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {
      def body(): Unit = {
        val (el, sel, pop) = blocking {
          global.cursor.step { implicit tx =>
            // if (DEBUG) println(s"iterate - inputAccess ${tx.inputAccess}")
            val cs    = genome.chromosomes()
            val fit   = genome.fitness()
            val all   = cs zip fit
            val _el   = elitism(all) // .toSet
            val _sel  = select (all) .map(tx.newHandle(_))
            (_el, Util.scramble(_sel.toIndexedSeq), all.size)
          }
        }
        val nGen    = pop - el.size - Algorithm.numGolem
        val nMut    = (Algorithm.mutationProb * nGen + 0.5).toInt
        val nCross  = nGen - nMut

        progress = 0.05
        checkAborted()

        // if (DEBUG) println(s"pop $pop, el ${el.size}, sel ${sel.size}, nMut $nMut, nCross $nCross")

        val mut     = blocking(mutate    (sel, nMut  ))
        progress = Algorithm.mutationProb * 0.55 + 0.05
        checkAborted()
        val cross   = blocking(crossover (sel, nCross))
        progress = 0.6
        checkAborted()

        val procEval = blocking {
          global.cursor.step { implicit tx =>
            cleaner.foreach { c =>
              c.apply(tx, el)
            }
            val golem = Vector.fill(Algorithm.numGolem)(mkIndividual())
            genome.chromosomes() = el ++ (mut ++ cross).map(_.apply()) ++ golem
            evaluateAndUpdate()
          }
        }
        await(procEval, offset = 0.6, weight = 0.4)
      }
    }
  }
}
