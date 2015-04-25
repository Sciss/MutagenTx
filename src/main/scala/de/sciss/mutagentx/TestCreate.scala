/*
 *  TestCreate.scala
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

object TestCreate extends App {
  val DEBUG = false
  val POP   = 50
  val ITER  = 100

  // de.sciss.lucre.confluent.showLog = true

  val base = file("database")
  require(base.isDirectory)
  val a = Algorithm(base / "test")
  a.global.cursor.step { implicit tx =>
    a.init(n = POP)
    a.evaluate()
    a.print()
  }
  for (i <- 1 to ITER) {
    println(s"----ITERATION $i ----")
    if (DEBUG) a.global.cursor.step { implicit tx => println(s"Input access = ${tx.inputAccess}") }
    a.iterate()
    if (i % 10 == 0) a.global.cursor.step { implicit tx =>
      a.print()
    }
  }

  a.system.close()
}
