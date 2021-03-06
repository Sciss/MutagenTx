/*
 *  MkSynthGraph.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx

import de.sciss.lucre.stm.Sys
import de.sciss.mutagentx.ParamRanges.Dynamic
import de.sciss.synth.ugen.{Constant, LeakDC}
import de.sciss.synth.{GE, SynthGraph, UGenSource, UGenSpec, UndefinedRate, ugen}

import scala.annotation.tailrec

object MkSynthGraph {
  /** Creates a synth graph from a chromosome, possibly inserting safety
    * measures such as removal of NaNs or protected against out-of-range parameters.
    *
    * @param c            the chromosome that shall be converted
    * @param mono         if `true`, adds a `Mix.mono`
    * @param removeNaNs   if `true`, adds a bad-value check and a gate that stops NaNs
    * @param config       if `true`, adds a `ConfigOut`
    * @param ranges       if `true`, inserts range protection checks for known parameters
    */
  def apply[S <: Sys[S]](c: Chromosome[S], mono: Boolean, removeNaNs: Boolean, config: Boolean, ranges: Boolean)
                        (implicit tx: S#Tx /*, random: TxnRandom[D#Tx] */): SynthGraph = {
    val top = c

    @tailrec def loop(rem: Vec[Vertex[S]], real: Map[Vertex[S], GE]): Map[Vertex[S], GE] = rem match {
      case init :+ last =>
        def isDynamic(in: GE): Boolean = in match {
          case Constant(_) => false
          case _ =>
            ParamRanges.map.get(Chromosome.elemName(in)).exists { info =>
              def getArg(name: String): Any = in.getClass.getMethod(name).invoke(in)

              def check(d: Dynamic): Boolean = d match {
                case Dynamic.Always => true
                case Dynamic.And(elems @ _*) => elems.forall(check)
                case Dynamic.Or (elems @ _*) => elems.exists(check)
                case Dynamic.IfOver (param, v) =>
                  getArg(param) match {
                    case Constant(f) if f >= v => true
                    case _ => false // XXX TODO --- could be more complex
                  }
                case Dynamic.IfUnder(param, v) =>
                  getArg(param) match {
                    case Constant(f) if f <= v => true
                    case _ => false // XXX TODO --- could be more complex
                  }
                case Dynamic.In(param) =>
                  getArg(param) match {
                    case argGE: GE => isDynamic(argGE)
                    case _ => false
                  }
              }

              info.dynamic.exists(check)
            }
        }

        lazy val lastE = top.targets(last)

        def getReal(name: String): Option[GE] =
          lastE.flatMap { e =>
            if (e.inlet == name) real.get(e.targetVertex) else None
          } .headOption

        val value: GE = last match {
          case vf: Vertex.Constant[S] => ugen.Constant(vf.f)
          case u: Vertex.UGen[S] =>
            val spec = u.info
            val ins = spec.args.map { arg =>
              val res: (AnyRef, Class[_]) = arg.tpe match {
                case UGenSpec.ArgumentType.Int =>
                  val v = arg.defaults.get(UndefinedRate) match {
                    case Some(UGenSpec.ArgumentValue.Int(i)) => i
                    case _ => 1 // rrand(1, 2)
                  }
                  (v.asInstanceOf[AnyRef], classOf[Int])

                case UGenSpec.ArgumentType.GE(_, _) =>
                  val inGEOpt0 = getReal(arg.name)

                  val inGEOpt: Option[GE] = if (!ranges) inGEOpt0 else inGEOpt0.map { inGE0 =>
                    ParamRanges.map.get(spec.name).fold(inGE0) { pInfo =>
                      pInfo.params.get(arg.name).fold(inGE0) { pSpec =>
                        val inInfoOpt = ParamRanges.map.get(Chromosome.elemName(inGE0))

                        lazy val inLoOpt: Option[Double] = inGE0 match {
                          case Constant(f) => Some(f.toDouble)
                          case _ => inInfoOpt.flatMap(_.outLo)
                        }
                        lazy val inHiOpt: Option[Double] = inGE0 match {
                          case Constant(f) => Some(f.toDouble)
                          case _ => inInfoOpt.flatMap(_.outHi)
                        }
                        val loThresh  = pSpec.lo.fold(Double.NegativeInfinity)(_.value)
                        val hiThresh  = pSpec.hi.fold(Double.PositiveInfinity)(_.value)
                        val loOk      = inLoOpt.exists(_ >= loThresh)
                        val hiOk      = inHiOpt.exists(_ <= hiThresh)

                        val inGE1: GE = (loOk, hiOk) match {
                          case (false, _ ) if !loThresh.isInfinity && (hiOk || hiThresh.isInfinity) =>
                            inGE0.max(loThresh)
                          case (_ , false) if !hiThresh.isInfinity && (loOk || loThresh.isInfinity) =>
                            inGE0.min(hiThresh)
                          case (false, false) if !hiThresh.isInfinity && !loThresh.isInfinity =>
                            // N.B. Clip.ar seems to be broken
                            // inGE0.clip(loThresh, hiThresh)
                            inGE0.max(loThresh).min(hiThresh)
                          case _ => inGE0
                        }
                        // `lessThan` -> see below

                        val inGE2: GE = if (!pSpec.dynamic || isDynamic(inGE1)) inGE1 else LeakDC.ar(inGE1)

                        inGE2
                      }
                    }
                  }

                  val inGE = inGEOpt.getOrElse {
                    val xOpt = arg.defaults.get(UndefinedRate)
                    val x    = xOpt.getOrElse {
                      val inc = Chromosome.findIncompleteUGenInputs[S](top, u)
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
                      case UGenSpec.ArgumentValue.Nyquist       => ugen.Nyquist()
                      case UGenSpec.ArgumentValue.String(v)     => UGenSource.stringArg(v)
                    }
                  }
                  (inGE, classOf[GE])
              }
              res
            }
            // now solve `lessThan`
            val ins1 = ParamRanges.map.get(spec.name).fold(ins) { info =>
              val opt = info.params.collectFirst {
                case (param, spec0) if spec0.lessThan.isDefined => (param, spec0.lessThan.get)
              }
              opt.fold(ins) { case (param, ref) =>
                val paramIdx  = spec.args.indexWhere(_.name == param)
                val refIdx    = spec.args.indexWhere(_.name == ref  )
                val paramIn   = ins(paramIdx)
                val refIn     = ins(refIdx)

                (paramIn._1, refIn._1) match {
                  case (Constant(paramC), Constant(refC)) =>
                    if (paramC <= refC) ins else ins.updated(paramIdx, refIn)
                  // case (ge, Constant(refC)) if findOutHi(ge) <= refC => ins  // too much effort, drop it
                  case (paramGE: GE, refGE: GE) =>
                    ins.updated(paramIdx, (paramGE min refGE, classOf[GE]))
                  case _ => ins
                }
              }
            }

            u.instantiate(ins1)
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
        case ugen: Vertex.UGen[S] => ugen
      }
      if (ugens.nonEmpty) {
        val roots = Chromosome.getRoots(top)
        val sig0: GE = if (roots.isEmpty) map(ugens.head /* choose(ugens) */) else Mix(roots.map(map.apply))
        val sig1  = /* if (mono) */ Mix.mono(sig0) /* else sig0 */
        val sig2  = if (!removeNaNs) sig1 else {
            val isOk = CheckBadValues.ar(sig1, post = 0) sig_== 0
            Gate.ar(sig1, isOk)
          }
        val sig3  = if (config) sig2 else Limiter.ar(LeakDC.ar(sig2))
        val sig   = if (mono)   sig3 else Pan2.ar(sig3) // SplayAz.ar(numChannels = 2, in = sig3)
        if (config)
          ConfigOut(sig)
        else
          Out.ar(0, sig)
      }
    }
  }
}
