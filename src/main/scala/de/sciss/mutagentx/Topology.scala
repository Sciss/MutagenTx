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

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr

import scala.language.higherKinds

object Topology {
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
  * @tparam V             vertex type
  * @tparam E             edge type
  */
trait Topology[S <: Sys[S], V, E <: Topology.Edge[V]] {
  def vertices: expr.List.Modifiable[S, V]
  def edges   : expr.List.Modifiable[S, E]
  // def unconnected: S#Var[Int]
  // def edgeMap: SkipList.Map[S, Int, Map[V, Set[E]]])

  def targets(v: V)(implicit tx: S#Tx): Set[E]
  def sources(v: V)(implicit tx: S#Tx): Set[E]

  import Topology.Move

  private type T = Topology[S, V, E]

  def debugString(implicit tx: S#Tx): String

  /** Validates the internal structure. If no errors are
    * found, an empty collection is returned, otherwise
    * a collection with all errors encountered is returned.
    */
  def validate()(implicit tx: S#Tx): Vec[String]

  /** For two connected vertices `a` and `b`, returns `-1` if `a` is before `b`, or `1` if `a` follows `b`,
    *  or `0` if both are equal. Throws an exception if `a` or `b` is unconnected.
    */
  def compare(a: V, b: V)(implicit tx: S#Tx): Int

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
  def addEdge(e: E)(implicit tx: S#Tx): Option[Move[V]]

  /** Tests if an edge can be added without producing a cycle.
    *
    * @param e  the edge to test
    * @return   `true` if the insertion is possible. Then calling `addEdge` is guaranteed to be a `Success`.
    *           `false` if the insertion would introduce a cycle. Then calling `addEdge` is guaranteed to be a
    *           `Failure`
    */
  def canAddEdge(e: E)(implicit tx: S#Tx): Boolean

  /** Removes the edge from the topology. If the edge is not contained in the
    * structure, throws an exception.
    */
  def removeEdge(e: E)(implicit tx: S#Tx): Unit

  /** Adds a new vertex to the set of unconnected vertices. Throws an exception
    * if the vertex had been added before.
    */
  def addVertex(v: V)(implicit tx: S#Tx): Unit

  /** Removes a vertex and all associated edges. If the vertex is not
    * contained in the structure, throws an exception
    *
    * Note: Automatically removes all edges
    */
  def removeVertex(v: V)(implicit tx: S#Tx): Unit
}