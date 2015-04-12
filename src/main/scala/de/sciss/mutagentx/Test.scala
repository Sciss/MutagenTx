package de.sciss.mutagentx

object Test extends App {
  val DEBUG = true
  val POP   = 4
  val ITER  = 4

  val a = Algorithm()
  a.global.cursor.step { implicit tx =>
    a.init(n = POP)
    a.evaluate()
    a.print()
  }
  for (i <- 1 to ITER) {
    println(s"----ITERATION $i ----")
    if (DEBUG) a.global.cursor.step { implicit tx => println(s"Input access = ${tx.inputAccess}") }
    a.iterate()
    a.global.cursor.step { implicit tx =>
      a.print()
    }
  }
}
