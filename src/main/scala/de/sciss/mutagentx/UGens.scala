package de.sciss.mutagentx

import de.sciss.synth.ugen.{EnvGen_Perc, EnvGen_Sine, EnvGen_Triangle, EnvGen_Linen, EnvGen_CutOff, EnvGen_DADSR, EnvGen_ASR, EnvGen_ADSR, UnaryOpUGen, BinaryOpUGen}
import de.sciss.synth.{UndefinedRate, UGenSpec, audio, demand}

import scala.collection.breakOut

object UGens {
  private val NoNoAttr: Set[UGenSpec.Attribute] = {
    import UGenSpec.Attribute._
    Set(HasSideEffect, ReadsBuffer, ReadsBus, ReadsFFT, WritesBuffer, WritesBus, WritesFFT)
  }

  private val RemoveUGens = Set[String](
    "MouseX", "MouseY", "MouseButton", "KeyState",
    "BufChannels", "BufDur", "BufFrames", "BufRateScale", "BufSampleRate", "BufSamples",
    "SendTrig", "SendReply", "CheckBadValues",
    "Demand", "DemandEnvGen", "Duty",
    "SubsampleOffset", // "Klang", "Klank", "EnvGen", "IEnvGen"
    "LocalIn" /* for now! */,
    "NumAudioBuses", "NumBuffers", "NumControlBuses", "NumInputBuses", "NumOutputBuses", "NumRunningSynths",
    "Free", "FreeSelf", "FreeSelfWhenDone", "PauseSelf", "PauseSelfWhenDone",
    "ClearBuf", "LocalBuf",
    "RandID", "RandSeed",
    "Rand", "ExpRand", "IRand",
    /* "A2K", */ "K2A" /* , "DC" */
  )

  // these have done-action side-effects but we require doNothing, so they are allowed
  private val AddUGens = Set[String]("DetectSilence", "LFGauss", "Line", "Linen", "XLine")

  private val ugens0: Vec[UGenSpec] = (UGenSpec.standardUGens.valuesIterator.filter { spec =>
    spec.attr.intersect(NoNoAttr).isEmpty && !RemoveUGens.contains(spec.name) && spec.outputs.nonEmpty &&
      !spec.rates.set.contains(demand)
  } ++ UGenSpec.standardUGens.valuesIterator.filter { spec => AddUGens.contains(spec.name) }).toIndexedSeq

  private val binUGens: Vec[UGenSpec] = {
    import BinaryOpUGen._
    val ops = Vector[Op](Plus, Minus, Times, Div, Mod, Eq, Neq, Lt, Gt, Leq, Geq, Min, Max, BitAnd, BitOr, BitXor,
      RoundTo, RoundUpTo, Trunc, Atan2, Hypot, Hypotx, Pow, Ring1, Ring2, Ring3, Ring4, Difsqr, Sumsqr, Sqrsum,
      Sqrdif, Absdif, Thresh, Amclip, Scaleneg, Clip2, Excess, Fold2, Wrap2
    )
    ops.map { op =>
      val name  = s"Bin_${op.id}"
      val rates = UGenSpec.Rates.Set(Set(audio))
      val arg1  = UGenSpec.Argument(name = "a", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Generic),
        defaults = Map.empty, rates = Map.empty)
      val arg2  = arg1.copy(name = "b")
      val in1   = UGenSpec.Input(arg = "a", tpe = UGenSpec.Input.Single)
      val in2   = in1.copy(arg = "b")
      val out   = UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None)
      UGenSpec(name = name, attr = Set.empty, rates = rates, args = Vec(arg1, arg2),
        inputs = Vec(in1, in2), outputs = Vec(out), doc = None)
    }
  }

  private val unaryUGens: Vec[UGenSpec] = {
    import UnaryOpUGen._
    val ops = Vector[Op](Neg, Not, Abs, Ceil, Floor, Frac, Signum, Squared, Cubed, Sqrt, Exp, Reciprocal,
      Midicps, Cpsmidi, Midiratio, Ratiomidi, Dbamp, Ampdb, Octcps, Cpsoct, Log, Log2, Log10, Sin, Cos,
      Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh, Distort, Softclip, Ramp, Scurve)
    ops.map { op =>
      val name  = s"Un_${op.id}"
      val rates = UGenSpec.Rates.Set(Set(audio))
      val arg1  = UGenSpec.Argument(name = "a", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Generic),
        defaults = Map.empty, rates = Map.empty)
      val in1   = UGenSpec.Input(arg = "a", tpe = UGenSpec.Input.Single)
      val out   = UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None)
      UGenSpec(name = name, attr = Set.empty, rates = rates, args = Vec(arg1),
        inputs = Vec(in1), outputs = Vec(out), doc = None)
    }
  }
  
  private val envUGens: Vec[UGenSpec] = {
    val out   = UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None)
    val arg1Gate  = UGenSpec.Argument(name = "gate", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Gate),
      defaults = Map(UndefinedRate -> UGenSpec.ArgumentValue.Int(1)), rates = Map.empty)
    val arg2LvlScl = UGenSpec.Argument(name = "levelScale", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Generic),
      defaults = Map(UndefinedRate -> UGenSpec.ArgumentValue.Float(1f)), rates = Map.empty)
    val arg3LvlBias = UGenSpec.Argument(name = "levelBias", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Generic),
      defaults = Map(UndefinedRate -> UGenSpec.ArgumentValue.Float(0f)), rates = Map.empty)
    val arg4TimeScl = UGenSpec.Argument(name = "timeScale", tpe = UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.Generic),
      defaults = Map(UndefinedRate -> UGenSpec.ArgumentValue.Float(1f)), rates = Map.empty)

    val genArgs   = Vector(arg1Gate, arg2LvlScl, arg3LvlBias, arg4TimeScl)

    val comp      = Vector(EnvGen_ADSR, EnvGen_ASR, EnvGen_CutOff, EnvGen_DADSR, EnvGen_Linen, EnvGen_Perc,
      EnvGen_Sine, EnvGen_Triangle)

    comp.map { c =>
      val args      = c.envelopeArgs ++ genArgs
      val inputs    = args.map(a => UGenSpec.Input(arg = a.name, tpe = UGenSpec.Input.Single))

      UGenSpec(name = c.productPrefix, attr = Set.empty,
        rates = UGenSpec.Rates.Implied(audio, UGenSpec.RateMethod.Custom("apply")),
        args = args, inputs = inputs, outputs = Vec(out), doc = None)
    }
  }

  val seq: Vec[UGenSpec] = ugens0 ++ binUGens ++ unaryUGens ++ envUGens

  val map: Map[String, UGenSpec] = seq.map(s => s.name -> s)(breakOut)

  // val index: Map[Int, UGenSpec] = seq.zipWithIndex.map(_.swap)(breakOut)
}
