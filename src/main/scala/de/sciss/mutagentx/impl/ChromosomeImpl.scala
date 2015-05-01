/*
 *  ChromosomeImpl.scala
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

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.synth.ugen.SampleRate
import de.sciss.synth.{UndefinedRate, UGenSpec, ugen, GE, SynthGraph}

import scala.annotation.tailrec

object ChromosomeImpl {
  def mkSynthGraph(c: Chromosome, mono: Boolean, removeNaNs: Boolean)
                  (implicit tx: S#Tx, random: TxnRandom[D#Tx]): SynthGraph = {
    import Util._

    val top = c

    @tailrec def loop(rem: Vec[Vertex], real: Map[Vertex, GE]): Map[Vertex, GE] = rem match {
      case init :+ last =>
        val value: GE = last match {
          case Vertex.Constant(f) => ugen.Constant(f)
          case u @ Vertex.UGen(spec) =>
            val ins = spec.args.map { arg =>
              val res: (AnyRef, Class[_]) = arg.tpe match {
                case UGenSpec.ArgumentType.Int =>
                  val v = arg.defaults.get(UndefinedRate) match {
                    case Some(UGenSpec.ArgumentValue.Int(i)) => i
                    case _ => rrand(1, 2)
                  }
                  (v.asInstanceOf[AnyRef], classOf[Int])

                case UGenSpec.ArgumentType.GE(_, _) =>
                  val lastE   = top.edgeMap.get(last)
                  val inGEOpt = lastE.flatMap { set =>
                    set.flatMap { e =>
                      if (e.inlet == arg.name) real.get(e.targetVertex) else None
                    } .headOption
                  }
                  val inGE = inGEOpt.getOrElse {
                    val xOpt = arg.defaults.get(UndefinedRate)
                    val x    = xOpt.getOrElse {
                      val inc = findIncompleteUGenInputs(top, u)
                      println("INCOMPLETE:")
                      inc.foreach(println)
                      println(top.debugString)
                      sys.error(s"Vertex $spec has no input for inlet $arg")
                    }
                    x match {
                      case UGenSpec.ArgumentValue.Boolean(v)    => ugen.Constant(if (v) 1 else 0)
                      case UGenSpec.ArgumentValue.DoneAction(v) => ugen.Constant(v.id)
                      case UGenSpec.ArgumentValue.Float(v)      => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Inf           => ugen.Constant(Float.PositiveInfinity)
                      case UGenSpec.ArgumentValue.Int(v)        => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Nyquist       => SampleRate.ir / 2
                      case UGenSpec.ArgumentValue.String(v)     => ugen.Escape.stringToGE(v)
                    }
                  }
                  (inGE, classOf[GE])
              }
              res
            }

            u.instantiate(ins)
        }

        loop(init, real + (last -> value))

      case _ =>  real
    }

    SynthGraph {
      import de.sciss.synth.ugen._
      RandSeed.ir()
      val vertices = top.vertices.iterator.toIndexedSeq
      val map   = loop(vertices, Map.empty)
      val ugens = vertices.collect {
        case ugen: Vertex.UGen => ugen
      }
      if (ugens.nonEmpty) {
        val roots = getRoots(top)
        val sig0: GE = if (roots.isEmpty) map(choose(ugens)) else Mix(roots.map(map.apply))
        val sig1  = /* if (mono) */ Mix.mono(sig0) /* else sig0 */
        val sig2  = if (!removeNaNs) sig1 else {
            val isOk = CheckBadValues.ar(sig1, post = 0) sig_== 0
            Gate.ar(sig1, isOk)
          }
        val sig3  = Limiter.ar(LeakDC.ar(sig2))
        val sig   = if (mono) sig3 else Pan2.ar(sig3) // SplayAz.ar(numChannels = 2, in = sig3)
        Out.ar(0, sig)
      }
    }
  }

  def findIncompleteUGenInputs(t1: Chromosome, v: Vertex.UGen)(implicit tx: S#Tx): Vec[String] = {
    val spec      = v.info
    val edgeSet   = t1.edgeMap.get(v).getOrElse(Set.empty)
    val argsFree  = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val inc       = argsFree.filterNot(_.defaults.contains(UndefinedRate))
    inc.map(_.name)
  }

  def geArgs(spec: UGenSpec): Vec[UGenSpec.Argument] = {
    val res       = spec.args.filter { arg =>
      arg.tpe match {
        case UGenSpec.ArgumentType.Int => false
        case UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.DoneAction, _) => false
        case _ => true
      }
    }
    res
  }

  private def getRoots(top: Chromosome)(implicit tx: S#Tx): Vec[Vertex.UGen] = {
    val ugens = top.vertices.iterator.collect {
      case ugen: Vertex.UGen => ugen
    }
    val edges = top.edges.iterator.toList
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq
  }
}
