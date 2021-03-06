/*
 *  ConfigOut.scala
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

package de.sciss.synth
package ugen

import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.UGenSource._

object ConfigOut {
  var CLIP          = false
  var NORMALIZE     = false
  var PAN2          = false
  var HPF           = false
  var LIMITER       = false
  var AMP           = false
  var FADEIN        = false
}
final case class ConfigOut(in: GE) extends UGenSource.ZeroOut with WritesBus {
  protected def makeUGens: Unit = unwrap(this, in.expand.outputs)

  private[synth] def makeUGen(ins: Vec[UGenIn]): Unit = {
    val sig0  = ins: GE
    val isOk  = CheckBadValues.ar(sig0, post = 0) sig_== 0
    val sig1  = Gate.ar(sig0, isOk)
    val sig2  = if (!ConfigOut.CLIP     ) sig1 else sig1.clip2(1)
    val sig3  = if (!ConfigOut.LIMITER  ) sig2 else Limiter.ar(LeakDC.ar(sig1), -0.2.dbamp)
    val sig4  = if (!ConfigOut.HPF      ) sig3 else HPF.ar(sig3, 20)
    val sig5  = if (!ConfigOut.NORMALIZE) sig4 else {
      val env = EnvGen.ar(Env.asr, gate = "gate".kr(1f), doneAction = doNothing /* freeSelf */)
      val doneEnv = Done.kr(env)
      val normDur = 2.0
      val tFree = TDelay.kr(doneEnv, normDur)
      FreeSelf.kr(tFree)
      Normalizer.ar(sig4, level = -0.2.dbamp, dur = normDur) * DelayN.ar(env, normDur, normDur)
    }
    val bus = "out".kr(0f)
    val sig6 = if (!ConfigOut.PAN2  ) sig5 else Pan2.ar(sig5)
    val sig7 = if (!ConfigOut.AMP   ) sig6 else sig6 * "amp".kr(1f)
    val sig  = if (!ConfigOut.FADEIN) sig7 else {
      val ln0 = Line.ar(start = 0, end = 1, dur = 0.05)
      val ln  = if (!ConfigOut.LIMITER) ln0 else DelayN.ar(ln0, 0.1, 0.1)
      sig7 * ln
    }
    Out.ar(bus, sig)
  }
}
