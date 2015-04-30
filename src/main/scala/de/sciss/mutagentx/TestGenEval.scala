package de.sciss.mutagentx

import de.sciss.file._

import scala.util.{Success, Failure}

object TestGenEval extends App {
  val in        = file("audio_work") / "Betanovuss150410_1Cut.aif"
  val algorithm = Algorithm.tmp(in)

  val cursor    = algorithm.global.cursor
  cursor.step { implicit tx =>
    algorithm.init(100)
  }
  val fut = cursor.step { implicit tx =>
    algorithm.evaluate()
  }

  val t = new Thread {
    override def run(): Unit = {
      this.synchronized(this.wait())
      algorithm.system.close()
      sys.exit()
    }
    start()
  }

  def unhang(): Unit = t.synchronized(t.notifyAll())

  import Algorithm.executionContext

  fut.onComplete {
    case Success(vec) =>
      println(vec.mkString(", "))
      unhang()
    case Failure(ex)  =>
      ex.printStackTrace()
      unhang()
  }
}
