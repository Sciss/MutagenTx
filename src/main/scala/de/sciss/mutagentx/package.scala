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

import de.sciss.synth.proc.{Durable, Confluent}

package object mutagentx {
  type S = Confluent
  type D = Durable

  implicit val chromosomeSerializer = Topology.serializer[Vertex, Edge]

  // type Top = Topology[Vertex, Edge]

  type Chromosome = Topology[Vertex, Edge]

  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq
}
