/*
 *  Topology.scala
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

package de.sciss.mutagentx

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.{data, expr}
import de.sciss.lucre.stm.{Identifiable, Mutable}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.tailrec
import scala.collection.mutable.{Set => MSet, Stack => MStack}

object Topology {
  private implicit def ord[V <: Identifiable[S#ID]]: data.Ordering[S#Tx, V] = anyOrd.asInstanceOf[Ord[V]]

  private val anyOrd = new Ord[Identifiable[S#ID]]

  private final class Ord[V <: Identifiable[S#ID]] extends data.Ordering[S#Tx, V] {
    def compare(a: V, b: V)(implicit tx: S#Tx): Int = {
      val aid = a.id
      val bid = b.id
      val ab  = aid.base
      val bb  = bid.base
      if (ab < bb) -1 else if (ab > bb) 1 else {
        @tailrec def loop(ap: S#Acc,  bp: S#Acc): Int =
          if (ap.isEmpty) {
            -1 // we rule out equality before; if (bp.isEmpty) 0 else -1
          } else if (bp.isEmpty) 1 else {
            val ah = ap.head.toInt
            val bh = bp.head.toInt
            if (ah < bh) -1 else if (ah > bh) 1 else {
              loop(ap.tail, bp.tail)
            }
          }

        if (aid.path == bid.path) 0 else loop(aid.path, bid.path)
      }
    }
  }

  /** Creates an empty topology with no vertices or edges.
    *
    * @tparam V   vertex type
    * @tparam E   edge type
    */
  def empty[V <: Identifiable[S#ID], E <: Edge[V]](implicit tx: S#Tx,
                                                   vertexSer: Serializer[S#Tx, S#Acc, V],
                                                   edgeSer  : Serializer[S#Tx, S#Acc, E]) = {
    val id = tx.newID()
    new Topology(id, expr.List.Modifiable[S, V], expr.List.Modifiable[S, E], tx.newIntVar(id, 0),
      SkipList.Map.empty[S, V, Set[E]] /* tx.newDurableIDMap[Set[E]] */)
  }

  implicit def serializer[V <: Identifiable[S#ID], E <: Edge[V]](implicit vertexSer: Serializer[S#Tx, S#Acc, V],
                                                                          edgeSer  : Serializer[S#Tx, S#Acc, E])
    : Serializer[S#Tx, S#Acc, Topology[V, E]] = new Ser[V, E]

  def read[V <: Identifiable[S#ID], E <: Edge[V]](in: DataInput, access: S#Acc)
                                                 (implicit tx: S#Tx, vertexSer: Serializer[S#Tx, S#Acc, V],
                                                                     edgeSer  : Serializer[S#Tx, S#Acc, E]) =
    serializer[V, E].read(in, access)

  private final class Ser[V <: Identifiable[S#ID], E <: Edge[V]](implicit vertexSer: Serializer[S#Tx, S#Acc, V],
                                                                          edgeSer  : Serializer[S#Tx, S#Acc, E])
    extends Serializer[S#Tx, S#Acc, Topology[V, E]] {

    def write(top: Topology[V, E], out: DataOutput): Unit = top.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Topology[V, E] = {
      val id          = tx.readID(in, access)
      val vertices    = expr.List.Modifiable.read[S, V](in, access)
      val edges       = expr.List.Modifiable.read[S, E](in, access)
      val unconnected = tx.readIntVar(id, in)
      val edgeMap     = SkipList.Map.read[S, V, Set[E]](in, access) // tx.readDurableIDMap[Set[E]](in)
      new Topology(id, vertices, edges, unconnected, edgeMap)
    }
  }

  trait Edge[+V] {
    def sourceVertex: V
    def targetVertex: V
  }

  sealed trait Move[V] {
    def reference: V
    def affected : Vec[V]
    def isAfter  : Boolean
    def isBefore : Boolean
  }
  final case class MoveAfter [V](reference: V, affected: Vec[V]) extends Move[V] {
    def isAfter = true ; def isBefore = false
  }
  final case class MoveBefore[V](reference: V, affected: Vec[V]) extends Move[V] {
    def isAfter = false; def isBefore = true
  }

  final case class CycleDetected() extends RuntimeException
}

/** An online topological order maintenance structure. This is an immutable data structure with
  * amortized costs. The edge adding operation returns a new copy of the modified structure along
  * with a list of vertices which have been moved due to the insertion. The caller can then use
  * that list to adjust any views (e.g. DSP processes).
  *
  * @param  vertices      the vertices in the structure
  * @param  edges         a set of edges between the vertices
  * @param  unconnected   the number of unconnected vertices (the leading elements in `vertices`)
  * @param  edgeMap       allows lookup of edges via vertex keys
  *
  * @tparam V             vertex type
  * @tparam E             edge type
  */
final class Topology[V <: Identifiable[S#ID], E <: Topology.Edge[V]] private (val id: S#ID,
                                                        val vertices: expr.List.Modifiable[S, V, Unit],
                                                        val edges   : expr.List.Modifiable[S, E, Unit],
                                                        val unconnected: S#Var[Int],
                                                        val edgeMap: SkipList.Map[S, V, Set[E]])
  extends Mutable.Impl[S] {

  import Topology.{CycleDetected, Move, MoveAfter, MoveBefore}

  private type T = Topology[V, E]

  override def toString() = s"Topology($id)" // s"Topology($vertices, $edges)($unconnected, $edgeMap)"

  def debugString(implicit tx: S#Tx): String = {
    val vs = vertices.iterator.toList.mkString("vertices = [", ", ", "]")
    val es = edges   .iterator.toList.mkString("edges    = [", ", ", "]")
    val em = edgeMap .iterator.toList.mkString("edgeMap  = [", ", ", "]")
    s"Topology($id,\n  $vs\n  $es\n  $em\n)"
  }

  protected def disposeData()(implicit tx: S#Tx): Unit = {
    vertices    .dispose()
    edges       .dispose()
    unconnected .dispose()
    edgeMap     .dispose()
  }

  protected def writeData(out: DataOutput): Unit = {
    vertices    .write(out)
    edges       .write(out)
    unconnected .write(out)
    edgeMap     .write(out)
  }

  /** For two connected vertices `a` and `b`, returns `-1` if `a` is before `b`, or `1` if `a` follows `b`,
    *  or `0` if both are equal. Throws an exception if `a` or `b` is unconnected.
    */
  def compare(a: V, b: V)(implicit tx: S#Tx): Int = {
    val ai = vertices.indexOf(a)
    val bi = vertices.indexOf(b)
    val u  = unconnected()
    require(ai >= u && bi >= u)
    if (ai < bi) -1 else if (ai > bi) 1 else 0
  }

  /** Tries to insert an edge into the topological order.
    * Throws an exception if the source or target vertex of the edge is not contained in the vertex list of this
    * structure.
    *
    * @param e  the edge to insert
    * @return   `Failure` if the edge would violate acyclicity, otherwise `Success` of a tuple
    *           that contains the new topology and possibly affected vertices which need to
    *           be moved with respect to the reference to reflect the new ordering. In case
    *           that the reference is the source vertex of the added edge, the affected vertices
    *           should be moved _after_ the reference and keep their internal grouping order.
    *           In case the reference is the target vertex, the affected vertices should be
    *           moved _before_ the reference
    */
  def addEdge(e: E)(implicit tx: S#Tx): Option[Move[V]] = {
    val source	   = e.sourceVertex
    val target	   = e.targetVertex
    val upBound	   = vertices.indexOf(source)
    if (upBound < 0) throw new IllegalArgumentException(s"Source vertex $source not found")
    val loBound	   = vertices.indexOf(target)
    if (loBound < 0) throw new IllegalArgumentException(s"Target vertex $target not found")
    if (loBound == upBound) throw new CycleDetected

    def succeed(): Unit = {
      // edgeMap.put(source.id, edgeMap.get(source.id).getOrElse(Set.empty) + e)
      edgeMap.add(source, edgeMap.get(source).getOrElse(Set.empty) + e)
      edges.addLast(e)
    }

    // dealing with unconnected elements
    val u = unconnected()
    if (upBound < u) { // first edge for source
      if (loBound < u) { // first edge for target
        val min         = math.min(upBound, loBound)
        val max         = math.max(upBound, loBound)
        val newUnCon    = u - 2
        unconnected()   = newUnCon
        vertices.removeAt(min)
        vertices.removeAt(max - 1)
        vertices.insert(newUnCon    , source)
        vertices.insert(newUnCon + 1, target)
        succeed()
        Some(MoveAfter(source, Vector(target)))
      } else {
        val newUnCon    = u - 1
        unconnected()   = newUnCon
        val sourceSeq   = Vector(source)
        vertices.removeAt(upBound)
        vertices.insert(loBound - 1, source)
        succeed()
        Some(MoveBefore(target, sourceSeq))
      }

      // regular algorithm
    } else if (loBound > upBound) {
      succeed()
      None

    } else /* if (loBound < upBound) */ {
      val visited = MSet.empty[V]
      if (!discovery(visited, source, e, target, upBound)) {
        throw new CycleDetected  // Cycle --> Abort
      } else {
        val affected = shift(visited, loBound, upBound)
        if (loBound < u) unconnected.transform(_ - 1)
        succeed()
        Some(MoveAfter(source, affected))
      }
    }
  }

  /** Tests if an edge can be added without producing a cycle.
    *
    * @param e  the edge to test
    * @return   `true` if the insertion is possible. Then calling `addEdge` is guaranteed to be a `Success`.
    *           `false` if the insertion would introduce a cycle. Then calling `addEdge` is guaranteed to be a
    *           `Failure`
    */
  def canAddEdge(e: E)(implicit tx: S#Tx): Boolean = {
    val source	   = e.sourceVertex
    val target	   = e.targetVertex
    val upBound	   = vertices.indexOf(source)
    val loBound	   = vertices.indexOf(target)

    (upBound >= 0 && loBound >= 0) && (upBound != loBound) && (
      (upBound < unconnected()) || (loBound > upBound) || {
        val visited = MSet.empty[V]
        discovery(visited, source, e, target, upBound)
      }
    )
  }

  /** Removes the edge from the topology. If the edge is not contained in the
    * structure, returns the topology unmodified.
    */
  def removeEdge(e: E)(implicit tx: S#Tx): Unit = {
    if (edges.remove(e)) {
      val source  = e.sourceVertex
      val newEMV  = edgeMap.get(source).getOrElse(Set.empty) - e
      if (newEMV.isEmpty) edgeMap.remove(source) else edgeMap.add(source, newEMV)
    }
  }

  /** Adds a new vertex to the set of unconnected vertices. Throws an exception
    * if the vertex had been added before.
    */
  def addVertex(v: V)(implicit tx: S#Tx): Unit = {
    val contains = vertices.indexOf(v) >= 0 // should have a `contains` method
    if (contains) throw new IllegalArgumentException(s"Vertex $v was already added")
    vertices.addHead(v)
    unconnected.transform(_ + 1)
  }

  /** Removes a vertex and all associated edges. If the vertex is not
    * contained in the structure, returns the unmodified topology.
    *
    * Note: Automatically removes outgoing edges, __but not incoming edges__
    */
  def removeVertex(v: V)(implicit tx: S#Tx): Unit = {
    val idx = vertices.indexOf(v)
    if (idx >= 0) {
      vertices.removeAt(idx)
      val u = unconnected()
      if (idx < u) {
        unconnected.transform(_ - 1)
      } else {
        edgeMap.get(v).foreach { e =>
          edgeMap.remove(v)
          e.foreach(edges.remove)
        }
      }
    }
  }

  // note: assumes audio rate
  private def discovery(visited: MSet[V], newV: V, newE: E, v0: V, upBound: Int)(implicit tx: S#Tx): Boolean = {
    val targets = MStack(v0)
    while (targets.nonEmpty) {
      val v           = targets.pop()
      visited        += v
      val m0          = edgeMap.get(v).getOrElse(Set.empty)
      val m1          = if (v == newV) m0 + newE else m0
      val moreTargets = m1.map(_.targetVertex)
      val grouped     = moreTargets.groupBy { t =>
        val vIdx = vertices.indexOf(t)
        if (vIdx < upBound) -1 else if (vIdx > upBound) 1 else 0
      }
      if (grouped.contains(0)) return false // cycle detected
      // visit s if it was not not already visited
      // and if it is in affected region
      //         grouped.get( -1 ).foreach( targets.pushAll( _.diff( visited )))
      targets.pushAll(grouped.getOrElse(-1, Set.empty).filter(!visited.contains(_)))
    }
    true
  }

  // initial cond: loBound (target) < upBound (source)
  private def shift(visited: collection.Set[V], loBound: Int, upBound: Int)(implicit tx: S#Tx): Vec[V] = {
    // shift vertices in affected region down ord
    val affected = (loBound until upBound).collect {
      case idx if visited.contains(vertices(idx)) => idx
    }
    affected.zipWithIndex.map { case (idx, numRemoved) =>
      val v = vertices.removeAt(idx - numRemoved)
      vertices.insert(upBound, v)
      v
    }
  }
}