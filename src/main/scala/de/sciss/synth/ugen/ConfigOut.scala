package de.sciss.synth.ugen

import de.sciss.synth._

import scala.collection.immutable.{IndexedSeq => Vec}

final case class ConfigOut(in: GE) extends UGenSource.ZeroOut with WritesBus {
  protected def makeUGens: Unit = unwrap(in.expand.outputs)

  protected def makeUGen(ins: Vec[UGenIn]): Unit = {
    val sig0  = ins: GE
    val isOk  = CheckBadValues.ar(sig0, post = 0) sig_== 0
    val sig1  = Gate.ar(sig0, isOk)
    val sig2  = Limiter.ar(LeakDC.ar(sig1), -0.2.dbamp)
    val sig   = HPF.ar(sig2, 20)
    val bus   = "out".kr(0f)
    Out.ar(bus, sig)
  }
}
