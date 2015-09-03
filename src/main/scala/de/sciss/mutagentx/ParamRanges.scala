package de.sciss.mutagentx

import scala.language.implicitConversions

object ParamRanges {
  implicit def wrapOption[A](in: A): Option[A] = Some(in)
  implicit def wrapMargin(in: Double): Option[Margin] = Some(Margin(in))

  case class Margin(value: Double, hard: Boolean = true) {
    def soft: Boolean = !hard
  }

  case class Spec(lo: Option[Margin] = None, hi: Option[Margin] = None, dcBlock: Boolean = false,
                  lessThan: Option[String] = None, scalar: Boolean = false)

  case class Info(params: Map[String, Spec] = Map.empty, outLo: Option[Double] = None, outHi: Option[Double] = None)

  val map: Map[String, Info] = Map(
    "GVerb" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "roomSize"      -> Spec(lo = 0.55, lessThan = "maxRoomSize"),  // lo!
      "revTime"       -> Spec(lo = 0.0, hi = 100.0 /* soft */),
      "damping"       -> Spec(lo = 0.0, hi = 1.0),
      "inputBW"       -> Spec(lo = 0.0, hi = 1.0),
      "spread"        -> Spec(lo = 0.0, hi = 43.0), // hi!
      // "dryLevel"      -> Spec(),
      // "earlyRefLevel" -> Spec(),
      // "tailLevel"     -> Spec(),
      "maxRoomSize"   -> Spec(lo = 0.55, hi = 300.0 /* soft */, scalar = true)
    )),
    "DelayN" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayL" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayC" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "CombN" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombL" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombC" -> Info(params = Map(
      "in"            -> Spec(dcBlock = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    ))
  )
}
