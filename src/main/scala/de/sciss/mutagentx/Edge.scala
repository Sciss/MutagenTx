package de.sciss.mutagentx

import de.sciss.serial.{DataOutput, DataInput, Serializer}

object Edge {
  implicit object Ser extends Serializer[S#Tx, S#Acc, Edge] {
    def write(e: Edge, out: DataOutput): Unit = {
      e.sourceVertex.write(out)
      e.targetVertex.write(out)
      // out.writeUTF(e.inlet)
      out.writeShort(e.inletIndex)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Edge = {
      val sourceVertex  = Vertex.UGen.read(in, access)
      val targetVertex  = Vertex.Ser .read(in, access)
      // val inlet         = in.readUTF()
      val inlet         = in.readShort()
      Edge(sourceVertex, targetVertex, inlet)
    }
  }

  def make(sourceVertex: Vertex.UGen, targetVertex: Vertex, inletName: String): Edge =
    apply(sourceVertex, targetVertex, sourceVertex.info.inputs.indexWhere(_.arg == inletName))
}
/** The edge points from '''consuming''' (source) element to '''input''' element (target).
  * Therefore, the `sourceVertex`'s `inlet` will be occupied by `targetVertex`
  */
case class Edge(sourceVertex: Vertex.UGen, targetVertex: Vertex, inletIndex: Int) extends Topology.Edge[Vertex] {
  lazy val inlet: String = sourceVertex.info.inputs(inletIndex).arg
}
