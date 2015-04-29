package de.sciss.mutagentx

import de.sciss.serial.{DataOutput, DataInput, Serializer}

object Edge {
  implicit object Ser extends Serializer[S#Tx, S#Acc, Edge] {
    def write(e: Edge, out: DataOutput): Unit = {
      e.sourceVertex.write(out)
      e.targetVertex.write(out)
      out.writeUTF(e.inlet)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Edge = {
      val sourceVertex  = Vertex.Ser.read(in, access)
      val targetVertex  = Vertex.Ser.read(in, access)
      val inlet         = in.readUTF()
      Edge(sourceVertex, targetVertex, inlet)
    }
  }
}
/** The edge points from '''consuming''' (source) element to '''input''' element (target).
  * Therefore, the `sourceVertex`'s `inlet` will be occupied by `targetVertex`
  */
case class Edge(sourceVertex: Vertex, targetVertex: Vertex, inlet: String) extends Topology.Edge[Vertex]
