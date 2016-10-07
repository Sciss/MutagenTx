/*
 *  SOMMinMax.scala
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

import java.io.{FileInputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.mutagentx.SOMGenerator.{SynthGraphDB, Weight}
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object SOMMinMax extends App {
  def numCoeff = 13

  run(args.headOption.getOrElse("betanovuss0"))

  import Algorithm.executionContext

  // spectral followed by temporal
  type Tpe = (Vec[(Double, Double)], Vec[(Double, Double)])

  def read(name: String): Tpe = {
    val statFile = file("database") / s"${name}_stat"
    val fin   = new FileInputStream(statFile)
    val sz    = fin.available()
    val szE   = numCoeff * 2 * 2 * 8 + 2 * 4
    require(sz == szE, s"Unexpected size $sz, expected $szE")
    val arr   = new Array[Byte](sz)
    fin.read(arr)
    fin.close()
    val din   = DataInput(arr)
    val ser   = implicitly[ImmutableSerializer[Tpe]]
    ser.read(din)
  }

  def run(name: String): Unit = {
    import kollflitz.Ops._

    val statFile = file("database") / s"${name}_stat"
    if (statFile.exists) {
      println(s"File $statFile already exists. Not regenerating.")
      return
    }

    val graphDB = SynthGraphDB.open(name)
    import graphDB._

    def perform(idx: Int, feature: Weight => Array[Double], print0: Boolean): (Double, Double) = {
      val all = system.step { implicit tx =>
        val res = Vector.newBuilder[Double]
        handle().iterator.foreach { li =>
          li.iterator.foreach { node =>
            // if (idx == 0 && node.input.fitness > 0.3) println(node.weight.temporal.mkString(", "))
            res += feature(node.weight)(idx)
          }
        }
        res.result()
      }
      val sorted = all.sortedT
      if (idx == 0 && print0) println(s"calcMinMax - based on ${sorted.size} samples.")
      (sorted.percentile(2), sorted.percentile(98))
    }

    val proc = Processor[Unit]("calc-min-max") { self =>
      def step(off: Double, feature: Weight => Array[Double], print0: Boolean): Vec[(Double, Double)] =
        (0 until numCoeff).map { idx =>
          val res = perform(idx, feature, print0 = print0)
          self.progress = ((idx + 1).toDouble / numCoeff) * 0.5 + off
          self.checkAborted()
          res
        }

      val statsSpectral = step(0.0, _.spectral, print0 = true )
      val statsTemporal = step(0.0, _.temporal, print0 = false)
      val stats = (statsSpectral, statsTemporal)

      val serializer  = implicitly[ImmutableSerializer[Tpe]]
      val fos         = new FileOutputStream(statFile)
      val dout        = DataOutput()
      serializer.write(stats, dout)
      fos.write(dout.toByteArray)
      fos.close()

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
      _ =>
        println("...complete")
        system.close()
        sys.exit()
    }
  }
}
