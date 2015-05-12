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
    val sig3  = HPF.ar(sig2, 20)
    val env   = EnvGen.ar(Env.asr, gate = "gate".kr(1f), doneAction = doNothing /* freeSelf */)
    val doneEnv = Done.kr(env)
    val normDur = 2.0
    val tFree = TDelay.kr(doneEnv, normDur)
    FreeSelf.kr(tFree)
    val sig   = Normalizer.ar(sig3 * env, level = -0.2.dbamp, dur = normDur)
    // env.poll(1, "env")
    // sig.poll(1, "sig")
    val bus   = "out".kr(0f)
    Out.ar(bus, sig)
  }
}
