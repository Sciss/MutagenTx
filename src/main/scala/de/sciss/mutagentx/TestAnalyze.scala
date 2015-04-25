package de.sciss.mutagentx

import de.sciss.file._

object TestAnalyze extends App {
  run()

  def run(): Unit = {
    val base = file("database")
    require(base.isDirectory)
    val a   = Algorithm(base / "test")
    val csr = a.global.cursor
    csr.step { implicit tx =>
      implicit val dtx = tx.durable
      println(s"path size = ${csr.position.size}")
      // a.print()
    }
    a.system.close()
  }
}
