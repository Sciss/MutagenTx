package de.sciss.mutagentx

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

class Evaluated[S <: Sys[S]](val chromosome: stm.Source[S#Tx, Chromosome[S]], val fitness: Double) {
  // def graph: SynthGraph = chromosome.graph

  // override def toString = f"[${graph.sources.size} sources; fitness = $fitness%1.2f]@${hashCode.toHexString}"
}