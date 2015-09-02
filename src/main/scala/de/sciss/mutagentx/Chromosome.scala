package de.sciss.mutagentx

import de.sciss.lucre.stm.{Elem, Sys}
import de.sciss.serial.{Serializer, DataInput}

object Chromosome extends Elem.Type {
  def typeID: Int = ???

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = ???

  def empty[S <: Sys[S]](implicit tx: S#Tx): Chromosome[S] = ???

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Chromosome[S]] = ???
}
trait Chromosome[S <: Sys[S]] extends Topology[S, Vertex[S], Edge[S]] with Elem[S]