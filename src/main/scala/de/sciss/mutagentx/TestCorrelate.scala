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
