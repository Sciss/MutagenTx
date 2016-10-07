/*
 *  TestCorrelate.scala
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

import de.sciss.file._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TestCorrelate extends App {
  val fBounce = userHome / "Music" / "work" / "muta_bnc4008684642804168185.aif"
  val fTarget = file("audio_work") / "derrida1_1_216500.aif"

  val fut = impl.EvaluationImpl.evaluateBounce(???, bounce = fBounce, input = fTarget)
  import Algorithm.executionContext
  val fit = Await.result(fut, Duration.Inf)
  println(s"Fitness: $fit")
}
