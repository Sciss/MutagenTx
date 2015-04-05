package de.sciss.mutagentx

object Test extends App {
  val a = Algorithm()
  a.global.cursor.step { implicit tx =>
    a.init(n = 10)
    a.evaluate()
    a.print()
  }
  for (i <- 1 to 4) {
    println(s"----ITERATION $i ----")
    a.global.cursor.step { implicit tx => println(s"Input access = ${tx.inputAccess}") }
    a.iterate()
    a.global.cursor.step { implicit tx =>
      a.print()
    }
  }
}
