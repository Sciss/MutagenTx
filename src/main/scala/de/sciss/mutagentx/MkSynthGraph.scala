package de.sciss.mutagentx

import de.sciss.lucre.stm.Sys
import de.sciss.synth.ugen.{LeakDC, Constant}
import de.sciss.synth.{UndefinedRate, UGenSpec, ugen, GE, SynthGraph}

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
                  val lastE   = top.targets(last) // top.edgeMap.get(last)
                  val inGEOpt0 = lastE.flatMap { e =>
                    if (e.inlet == arg.name) real.get(e.targetVertex) else None
                  } .headOption

                  val inGEOpt: Option[GE] = inGEOpt0.map { inGE0 =>
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
                          case (true , true ) => inGE0
                          case (false, true ) => inGE0.max(loThresh)
                          case (true , false) => inGE0.min(hiThresh)
                          case (false, false) => inGE0.clip(loThresh, hiThresh)
                        }
                        // XXX TODO -- `lessThan`

                        val inGE2: GE = if (!pSpec.dynamic) inGE1 else {
                          // XXX TODO --- add `dynamic` to ParamRanges.Info
                          LeakDC.ar(inGE1)
                        }

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
