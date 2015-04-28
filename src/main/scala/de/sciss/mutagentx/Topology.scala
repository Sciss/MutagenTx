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

import de.sciss.lucre.data
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr
import de.sciss.serial.Serializer

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable.{Set => MSet, Stack => MStack}
import scala.util.{Success, Failure, Try}

object Topology {
  /** Creates an empty topology with no vertices or edges.
    *
    * @tparam V   vertex type
    * @tparam E   edge type
    */
  def empty[V, E <: Edge[V]](implicit tx: S#Tx, vertexSer: Serializer[S#Tx, S#Acc, V],
                             edgeSer: Serializer[S#Tx, S#Acc, E]) = {
    val id = tx.newID()
    implicit val vertexOrd: data.Ordering[S#Tx, V] = ???
    implicit val edgeOrd  : data.Ordering[S#Tx, E] = ???
    new Topology(id, expr.List.Modifiable[S, V], SkipList.Set.empty[S, E], tx.newIntVar(id, 0),
      SkipList.Map.empty[S, V, Set[E]])
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
final class Topology[V, E <: Topology.Edge[V]] private (val id: S#ID,
                                                        val vertices: expr.List.Modifiable[S, V, Unit],
                                                        val edges: SkipList.Set[S, E],
                                                        val unconnected: S#Var[Int],
                                                        val edgeMap: SkipList.Map[S, V, Set[E]])
  /* extends Ordering[V] */ {

  import Topology.{Move, CycleDetected, MoveAfter, MoveBefore}

  private type T = Topology[V, E]

  override def toString = s"Topology($id)" // s"Topology($vertices, $edges)($unconnected, $edgeMap)"

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
  def addEdge(e: E)(implicit tx: S#Tx): Try[Option[Move[V]]] = {
    val source	   = e.sourceVertex
    val target	   = e.targetVertex
    val upBound	   = vertices.indexOf(source)
    if (upBound < 0) return Failure(new IllegalArgumentException(s"Source vertex $source not found"))
    val loBound	   = vertices.indexOf(target)
    if (loBound < 0) return Failure(new IllegalArgumentException(s"Target vertex $target not found"))
    if (loBound == upBound) Failure(new CycleDetected)

    def succeed(): Unit = {
      edgeMap.add(source -> (edgeMap.get(source).getOrElse(Set.empty) + e))
      edges.add(e)
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
        Success(Some(MoveAfter(source, Vector(target))))
      } else {
        val newUnCon    = u - 1
        unconnected()   = newUnCon
        val sourceSeq   = Vector(source)
        vertices.removeAt(upBound)
        vertices.insert(loBound - 1, source)
        succeed()
        Success(Some(MoveBefore(target, sourceSeq)))
      }

      // regular algorithm
    } else if (loBound > upBound) {
      succeed()
      Success(None)
    } else /* if (loBound < upBound) */ {
      val visited = MSet.empty[V]
      if (!discovery(visited, source, e, target, upBound)) {
        Failure(new CycleDetected)  // Cycle --> Abort
      } else {
        val (newVertices, affected) = shift(visited, loBound, upBound)
        if (loBound < u) unconnected.transform(_ - 1)
        succeed()
        Success(Some(MoveAfter(source, affected)))
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
      if (newEMV.isEmpty) edgeMap.remove(source) else edgeMap.add(source -> newEMV)
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
    */
  def removeVertex(v: V)(implicit tx: S#Tx): Unit = {
    val idx = vertices.indexOf(v)
    if (idx >= 0) {
      vertices.removeAt(idx)
      val u = unconnected()
      if (idx < u) {
        unconnected.transform(_ - 1)
      } else {
        if (edgeMap.contains(v)) {
          val e = edgeMap.get(v).getOrElse(Set.empty)
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
  private def shift(visited: collection.Set[V], loBound: Int, upBound: Int): (Vec[V], Vec[V]) = {
    ???
//    // shift vertices in affected region down ord
//    val (a, b)                  = vertices.splitAt(upBound)
//    val (begin, target)         = a.splitAt(loBound)
//    val source                  = b.head
//    val end                     = b.tail
//    val (affected, unaffected)  = target.partition(visited.contains)
//
//    val shifted = begin ++ unaffected ++ (source +: affected) ++ end
//
//    (shifted, affected)
  }
}