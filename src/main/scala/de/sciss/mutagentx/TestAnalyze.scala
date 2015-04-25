/*
 *  TestAnalyze.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

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
