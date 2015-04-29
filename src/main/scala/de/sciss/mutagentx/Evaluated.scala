package de.sciss.mutagentx

import de.sciss.lucre.stm

class Evaluated(val chromosome: stm.Source[S#Tx, Chromosome], val fitness: Double) {
  // def graph: SynthGraph = chromosome.graph

  // override def toString = f"[${graph.sources.size} sources; fitness = $fitness%1.2f]@${hashCode.toHexString}"
}