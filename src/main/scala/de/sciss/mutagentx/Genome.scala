package de.sciss.mutagentx

import de.sciss.lucre.data.SkipList

object Genome {

}
trait Genome {
  // def chromosomes: SkipList.Set[S, Chromosome]
  def chromosomes(implicit tx: S#Tx): Set[Chromosome]
}
