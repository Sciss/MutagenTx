package de.sciss.mutagentx

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm.{Elem, InMemory, Sys}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object Edge extends Elem.Type {
  def typeID: Int = ???

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
    ???
  }

  implicit def Ser[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Edge[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Edge[S]] {
    def write(e: Edge[S], out: DataOutput): Unit = {
      e.sourceVertex.write(out)
      e.targetVertex.write(out)
      // out.writeUTF(e.inlet)
      out.writeShort(e.inletIndex)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Edge[S] = {
      val sourceVertex  = Vertex.UGen.read(in, access)
      val targetVertex  = Vertex.Ser .read(in, access)
      // val inlet         = in.readUTF()
      val inlet         = in.readShort()
      Edge[S](sourceVertex, targetVertex, inlet)
    }
  }

  def make[S <: Sys[S]](sourceVertex: Vertex.UGen[S], targetVertex: Vertex[S], inletName: String): Edge[S] =
    apply[S](sourceVertex, targetVertex, sourceVertex.info.inputs.indexWhere(_.arg == inletName))
}
/** The edge points from '''consuming''' (source) element to '''input''' element (target).
  * Therefore, the `sourceVertex`'s `inlet` will be occupied by `targetVertex`
  */
case class Edge[S <: Sys[S]](sourceVertex: Vertex.UGen[S], targetVertex: Vertex[S], inletIndex: Int)
  extends Topology.Edge[Vertex[S]] with Elem[S] {

  def tpe: Elem.Type = Edge

  def write(out: DataOutput): Unit = ???

  def dispose()(implicit tx: S#Tx): Unit = ()

  def changed: EventLike[S, Any] = de.sciss.lucre.event.Dummy[S, Any]

  def copy1(sourceVertex: Vertex.UGen[S]  = sourceVertex,
            targetVertex: Vertex[S]       = targetVertex,
            inletIndex  : Int             = inletIndex): Edge[S] =
    new Edge(sourceVertex = sourceVertex, targetVertex = targetVertex, inletIndex = inletIndex)

  lazy val inlet: String = sourceVertex.info.inputs(inletIndex).arg
}