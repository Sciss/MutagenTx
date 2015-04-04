package de.sciss.mutagentx

import de.sciss.lucre.stm.Identifiable

object Edge {

}
trait Edge extends Identifiable[S#Tx] {
  def source: S#Var[Vertex]
  def target: S#Var[Vertex]
}
