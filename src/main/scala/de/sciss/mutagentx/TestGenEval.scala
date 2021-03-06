/*
 *  TestGenEval.scala
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

import scala.util.{Success, Failure}

object TestGenEval extends App {
  val in        = file("audio_work") / "Betanovuss150410_1Cut.aif"
  val algorithm = Algorithm.tmpConfluent(??? /* in */)

  import Algorithm.executionContext

  val cursor    = algorithm.global.cursor
  val fut0 = cursor.step { implicit tx =>
    algorithm.initialize()
  }
  val fut1 = fut0.map { _ =>
    cursor.step { implicit tx =>
      algorithm.evaluateAndUpdate()
    }
  }
  val fut = fut1.map { _ =>
    algorithm.iterate()
  }

  val t = new Thread {
    override def run(): Unit = {
      this.synchronized(this.wait())
      algorithm.close()
      sys.exit()
    }
    start()
  }

  def unhang(): Unit = t.synchronized(t.notifyAll())

  fut.onComplete {
    case Success(vec) =>
      // println(vec.mkString(", "))
      println("Success.")
      unhang()
    case Failure(ex)  =>
      ex.printStackTrace()
      unhang()
  }
}
