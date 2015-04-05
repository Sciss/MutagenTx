package de.sciss.mutagentx

object Test extends App {
  val a = Algorithm()
  a.global.cursor.step { implicit tx =>
    a.init(n = 20)
    a.evaluate()
    a.print()
  }
}
