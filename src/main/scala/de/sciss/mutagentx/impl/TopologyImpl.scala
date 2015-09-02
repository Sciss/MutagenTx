package de.sciss.mutagentx
package impl

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr
import de.sciss.lucre.stm.Sys
import de.sciss.serial.DataOutput

import scala.collection.mutable.{Set => MSet, Stack => MStack}

trait TopologyImpl[S <: Sys[S], V, E <: Topology.Edge[V]] extends Topology[S, V, E] {

  // ---- abstract ----

  /** the vertices in the structure */
  def vertices: expr.List.Modifiable[S, V]
  /** a set of edges between the vertices */
  def edges   : expr.List.Modifiable[S, E]
  /** the number of unconnected vertices (the leading elements in `vertices`) */
  def unconnected: S#Var[Int]
  /** allows lookup of edges via vertex keys */
  def edgeMap: SkipList.Map[S, Int, Map[V, Set[E]]]

  // ---- impl ----

  import Topology.{CycleDetected, Move, MoveAfter, MoveBefore}

  private type T = Topology[S, V, E]

  // override def toString = s"Topology($id)" // s"Topology($vertices, $edges)($unconnected, $edgeMap)"

  def edgeSet(v: V)(implicit tx: S#Tx): Set[E] =
    edgeMap.get(v.hashCode()).fold(Set.empty[E]) { m =>
      m.getOrElse(v, Set.empty)
    }

  def debugString(implicit tx: S#Tx): String = {
    val vs = vertices.iterator.toList.mkString("vertices = [", ", ", "]")
    val es = edges   .iterator.toList.mkString("edges    = [", ", ", "]")
    val em = edgeMap .iterator.toList.mkString("edgeMap  = [", ", ", "]")
    s"Topology($vs\n  $es\n  $em\n)"
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
      val key = source.hashCode()
      val m0 = edgeMap.get(key).getOrElse(Map.empty)
      val s0 = m0.getOrElse(source, Set.empty)
      val s1 = s0 + e
      val m1 = m0 + (source -> s1)
      edgeMap.add(key, m1)
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
        if (loBound < u) unconnected() = unconnected() - 1
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
      val key     = source.hashCode()
      val m0      = edgeMap.get(key).getOrElse(Map.empty)
      val s0      = m0.getOrElse(source, Set.empty)
      val s1      = s0 - e
      val m1      = if (s1.isEmpty) m0 - source else m0 + (source -> s1)
      if (m1.isEmpty) edgeMap.remove(key) else edgeMap.add(key, m1)
    }
  }

  /** Adds a new vertex to the set of unconnected vertices. Throws an exception
    * if the vertex had been added before.
    */
  def addVertex(v: V)(implicit tx: S#Tx): Unit = {
    val contains = vertices.indexOf(v) >= 0 // should have a `contains` method
    if (contains) throw new IllegalArgumentException(s"Vertex $v was already added")
    vertices.addHead(v)
    unconnected() = unconnected() + 1
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
        unconnected() = unconnected() - 1
      } else {
        val key = v.hashCode()
        edgeMap.get(key).foreach { m0 =>
          m0.get(v).foreach { e =>
            val m1 = m0 - v
            if (m1.isEmpty) edgeMap.remove(key) else edgeMap.add(key, m1)
            e.foreach(edges.remove)
          }
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
      val key         = v.hashCode()
      val m0          = edgeMap.get(key).getOrElse(Map.empty)
      val s0          = m0.getOrElse(v, Set.empty)
      val s1          = if (v == newV) s0 + newE else s0
      val moreTargets = s1.map(_.targetVertex)
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