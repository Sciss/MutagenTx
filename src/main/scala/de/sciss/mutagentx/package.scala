/*
 *  package.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss

import de.sciss.lucre.data
import de.sciss.lucre.stm.Sys

package object mutagentx {
  // type S = Confluent
  // type D = Durable

  implicit def chromosomeSerializer[S <: Sys[S]](implicit ord: data.Ordering[S#Tx, Vertex[S]]) =
    Topology.serializer[S, Vertex, Edge]

  // type Top = Topology[Vertex, Edge]

  type Chromosome[S <: Sys[S]] = Topology[S, Vertex[S], Edge[S]]
  val  Chromosome = Topology

  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  type Chromosomes[S <: Sys[S]] = Vec[Chromosome[S]]

  def init(): Unit = {
    Vertex.init()
    Edge  .init()
  }
}
