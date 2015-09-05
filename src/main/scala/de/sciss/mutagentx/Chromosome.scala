package de.sciss.mutagentx

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{NoSys, Obj, Sys}
import de.sciss.mutagentx.impl.ChromosomeImpl
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.GE
import impl.{ChromosomeImpl => Impl}

object Chromosome extends Obj.Type {
  final val typeID = 0x40000

  def getRoots[S <: Sys[S]](top: Chromosome[S])(implicit tx: S#Tx): Vec[Vertex.UGen[S]] = Impl.getRoots(top)

  def findIncompleteUGenInputs[S <: Sys[S]](c: Chromosome[S], v: Vertex.UGen[S])(implicit tx: S#Tx): Vec[String] =
    Impl.findIncompleteUGenInputs(c, v)

  def elemName(in: GE): String = Impl.elemName(in)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val cookie      = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, found $cookie, expected 3")
    val id          = tx.readID(in, access)
    val vertices    = expr.List.Modifiable.read[S, Vertex[S]](in, access)
    val edges       = expr.List.Modifiable.read[S, Edge  [S]](in, access)
    val unconnected = tx.readIntVar(id, in)
    val srcEdgeMap  = SkipList.Map.read[S, Int, Map[Vertex[S], Set[Edge[S]]]](in, access)
    val tgtEdgeMap  = SkipList.Map.read[S, Int, Map[Vertex[S], Set[Edge[S]]]](in, access)
    new ChromosomeImpl[S](id, vertices, edges, unconnected, srcEdgeMap, tgtEdgeMap)
  }

  def empty[S <: Sys[S]](implicit tx: S#Tx): Chromosome[S] = {
    val id          = tx.newID()
    val vertices    = expr.List.Modifiable[S, Vertex]
    val edges       = expr.List.Modifiable[S, Edge  ]
    val unconnected = tx.newIntVar(id, 0)
    val srcEdgeMap  = SkipList.Map.empty[S, Int, Map[Vertex[S], Set[Edge[S]]]]
    val tgtEdgeMap  = SkipList.Map.empty[S, Int, Map[Vertex[S], Set[Edge[S]]]]
    new ChromosomeImpl[S](id, vertices, edges, unconnected, srcEdgeMap, tgtEdgeMap)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Chromosome[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Chromosome[S]] {
    def tpe = Chromosome
  }
}
trait Chromosome[S <: Sys[S]] extends Topology[S, Vertex[S], Edge[S]] with Obj[S] {
  def tpe: Obj.Type = Chromosome
}