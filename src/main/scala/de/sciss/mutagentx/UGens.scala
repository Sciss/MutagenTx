package de.sciss.mutagentx

import de.sciss.synth.ugen.BinaryOpUGen
import de.sciss.synth.{UGenSpec, audio, demand}

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
    val ops = Vector(Plus, Minus, Times, Div, Mod, Eq, Neq, Lt, Gt, Leq, Geq, Min, Max, BitAnd, BitOr, BitXor,
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
      UGenSpec.apply(name = name, attr = Set.empty, rates = rates, args = Vec(arg1, arg2),
        inputs = Vec(in1, in2), outputs = Vec(out), doc = None)
    }
  }

  val seq: Vec[UGenSpec] = ugens0 ++ binUGens

  val map: Map[String, UGenSpec] = seq.map(s => s.name -> s)(breakOut)
}
