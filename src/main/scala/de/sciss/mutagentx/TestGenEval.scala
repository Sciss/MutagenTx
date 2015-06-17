package de.sciss.mutagentx

import de.sciss.file._

import scala.util.{Success, Failure}

object TestGenEval extends App {
  val in        = file("audio_work") / "Betanovuss150410_1Cut.aif"
  val algorithm = Algorithm.tmpConfluent(in)

  import Algorithm.executionContext

  val cursor    = algorithm.global.cursor
  cursor.step { implicit tx =>
    algorithm.init(100)
  }
  val fut0 = cursor.step { implicit tx =>
    algorithm.evaluateAndUpdate()
  }
  val fut = fut0.map { _ =>
    algorithm.iterate()
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
