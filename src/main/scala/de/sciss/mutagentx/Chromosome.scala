package de.sciss.mutagentx

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm.Identifiable

object Chromosome {

}
trait Chromosome extends Identifiable[S#ID] {
  def vertices: SkipList.Set[S, Vertex]
  def edges   : SkipList.Set[S, Edge  ]
  def fitness : S#Var[Double]
}