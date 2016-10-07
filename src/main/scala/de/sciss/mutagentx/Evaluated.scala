/*
 *  Evaluated.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

class Evaluated[S <: Sys[S]](val chromosome: stm.Source[S#Tx, Chromosome[S]], val fitness: Double) {
  // def graph: SynthGraph = chromosome.graph

  // override def toString = f"[${graph.sources.size} sources; fitness = $fitness%1.2f]@${hashCode.toHexString}"
}