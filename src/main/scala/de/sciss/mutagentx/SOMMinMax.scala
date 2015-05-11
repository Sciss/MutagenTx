package de.sciss.mutagentx

import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.mutagentx.SOMGenerator.{Weight, SynthGraphDB}
import de.sciss.processor.Processor

object SOMMinMax extends App {
  def numCoeff = 13

  run(args.headOption.getOrElse("betanovuss0"))

  import Algorithm.executionContext

  def run(name: String): Unit = {
    import kollflitz.Ops._

    val graphDB = SynthGraphDB.open(name)
    import graphDB._

    def perform(idx: Int, feature: Weight => Array[Double]): (Double, Double) = {
      val all = system.step { implicit tx =>
        val res = Vector.newBuilder[Double]
        handle().iterator.foreach { li =>
          li.iterator.foreach { node =>
            // println(node.weight.temporal.mkString(", "))
            res += feature(node.weight)(idx)
          }
        }
        res.result()
      }
      val sorted = all.sortedT
      if (idx == 0) println(s"calcMinMax - based on ${sorted.size} samples.")
      (sorted.percentile(2), sorted.percentile(98))
    }

    val proc = Processor[Unit]("calc-min-max") { self =>
      def step(off: Double, feature: Weight => Array[Double]): Vec[(Double, Double)] =
        (0 until numCoeff).map { idx =>
          val res = perform(idx, feature)
          self.progress = ((idx + 1).toDouble / numCoeff) * 0.5 + off
          self.checkAborted()
          res
        }

      val statsSpectral = step(0.0, _.spectral)
      val statsTemporal = step(0.0, _.temporal)

      def print(name: String, stats: Vec[(Double, Double)]): Unit = {
        println(s"------ STATS FOR $name ------")
        println(" 2% percentiles:")
        println(stats.map(tup => f"${tup._1}%1.4f").mkString("[", ",", "]"))
        println("98% percentiles:")
        println(stats.map(tup => f"${tup._2}%1.4f").mkString("[", ",", "]"))
      }

      print("spectral", statsSpectral)
      print("temporal", statsTemporal)
    }

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }

    proc.monitor()
    proc.onFailure {
      case ex =>
        println("synth def database generation failed:")
        ex.printStackTrace()
    }

    proc.onComplete {
      case _ =>
        println("...complete")
        system.close()
        sys.exit()
    }
  }
}
