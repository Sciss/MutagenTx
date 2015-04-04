package de.sciss.mutagentx

import de.sciss.lucre.stm.Identifiable

object Vertex {
  // strangely, a constant is mutable, while a ugen is constant

  trait Constant extends Vertex with Identifiable[S#Tx] {
    def value: S#Var[Double]
  }

  trait UGen extends Vertex {
    def name: String
  }
}
sealed trait Vertex {

}
