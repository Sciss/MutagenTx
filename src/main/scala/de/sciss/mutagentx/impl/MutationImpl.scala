/*
 *  MutationImpl.scala
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
package impl

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.{confluent, stm}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}

object MutationImpl {
  private val stats = Array.fill(7)(0)

  def printStats(): Unit = println(stats.mkString(", "))

  private def getTargets(top: Chromosome, v: Vertex)(implicit tx: S#Tx): Set[Edge] =
    top.edges.iterator.collect {
      case e @ Edge(_, `v`, _) => e // a vertex `vi` that uses the removed vertex as one of its inlets
    } .toSet

  def removeVertex1(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Vertex = {
    implicit val dtx = tx.durable
    val vertices    = top.vertices
    val numVertices = vertices.size
    val idx         = random.nextInt(numVertices)
    val v           = vertices(idx)
    val targets     = getTargets(top, v)
    top.removeVertex(v)
    targets.foreach { e =>
      // val x = top2.removeEdge(e)
      // assert(x ne top2)
      top.removeEdge(e)
    }
    targets.foreach { case Edge(t: Vertex.UGen, _, _) =>
      ChromosomeImpl.completeUGenInputs(top, t)
    }
    v
  }

  /** Produces a sequence of `n` items by mutating the input `sq` selection.
    *
    * It assumes the invoking transaction is 'up-to-date' and will cause
    * the selection's cursors to step from this transaction's input access.
    */
  def apply(algorithm: Algorithm, sq: Vec[stm.Source[S#Tx, Chromosome]], n: Int,
            inputAccess: S#Acc): Vec[(S#Acc, confluent.Source[S, Chromosome])] = {
    var res = Vector.empty[(S#Acc, confluent.Source[S, Chromosome])]

    while (res.size < n) {
      val chosenH = sq(res.size % sq.size)
      val csr     = algorithm.global.forkCursor
      val hOpt    = csr.stepFrom(inputAccess) { implicit tx =>
        import algorithm.global.{rng => random}
        implicit val dtx = tx.durable
        val chosen        = chosenH()
        val mutationIter  = Util.rrand(Algorithm.mutMin, Algorithm.mutMax)
        require(mutationIter > 0)
        val success       = (false /: (1 to mutationIter)) { case (success0, iter) =>
          val success1 = (random.nextInt(7): @switch) match {
            case 0 => addVertex   (chosen)
            case 1 => removeVertex(chosen)
            case 2 => changeVertex(chosen)
            case 3 => changeEdge  (chosen)
            case 4 => swapEdge    (chosen)
            case 5 => splitVertex (chosen)
            case 6 => mergeVertex (chosen)
          }
          success0 | success1 // i.e. at least one mutation was applied
        }
        if (success) Some(tx.newHandleM(chosen)) else None
      }
      hOpt.foreach { h =>
        val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
        if (Algorithm.DEBUG) println(s"$h - $pos")
        res :+= (pos, h)
      }
    }
    res
  }

  private def roulette[A](in: Vec[(A, Int)])(implicit tx: S#Tx, random: TxnRandom[D#Tx]): A = {
    implicit val dtx = tx.durable
    val sum         = in.map(_._2).sum
    val norm        = in.zipWithIndex.map { case ((c, f), j) => (j, f / sum) }
    val sorted      = norm.sortBy(_._2)
    val accum       = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
    val roul        = random.nextDouble() // * max
    val idxS        = accum.indexWhere(_ > roul)
    val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
    val (chosen, _) = in(idx)
    chosen
  }

  private def addVertex(c: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    import Algorithm.maxNumVertices
    if (c.vertices.size >= maxNumVertices) false else {
      ChromosomeImpl.addVertex(c)
      checkComplete(c, s"addVertex()")
      stats(0) += 1
      true
    }
  }

  private def removeVertex(c: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    import Algorithm.minNumVertices
    val vertices    = c.vertices
    val numVertices = vertices.size
    if (numVertices <= minNumVertices) false else {
      removeVertex1(c)
      checkComplete(c, s"removeVertex($c)")
      stats(1) += 1
      true
    }
  }

  private def changeVertex(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    implicit val dtx = tx.durable

    val vertices    = top.vertices
    val numVertices = vertices.size

    val idx     = random.nextInt(numVertices)
    val vOld    = vertices(idx)
    vOld match {
      case f: Vertex.Constant => changeVertexConstant(top, f)
      case u: Vertex.UGen     => changeVertexUGen    (top, u)
    }
    stats(2) += 1

    true
  }

  private def changeVertexConstant(top: Chromosome, vc: Vertex.Constant)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Unit = {
    val fNew: Float = if (Util.coin())
      ChromosomeImpl.mkConstantValue()            // completely random
    else
      vc.f() * Util.exprand(0.9, 1.0/0.9).toFloat  // gradual change

    vc.f() = fNew
  }

  private def changeVertexUGen(top: Chromosome, vu: Vertex.UGen)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Unit = {
    val outlet  = getTargets(top, vu)
    val inlets  = top.edgeMap.get(vu).getOrElse(Set.empty)
    outlet.foreach(top.removeEdge)
    inlets.foreach(top.removeEdge)
    top.removeVertex(vu)

    val vNew = ChromosomeImpl.mkUGen()

    val oldInletNames: Vec[String] = vu match {
      case Vertex.UGen(info) => /* ChromosomeImpl.geArgs(info).map(_.name) */ info.inputs.map(_.arg)
      case _ => Vec.empty
    }

    top.addVertex(vNew)
    outlet.map(_.copy(targetVertex = vNew)).foreach(top.addEdge /* .get */)

    // just as many as possible, leaving tail inlets empty
    val newInlets = vNew match {
      case vNewU: Vertex.UGen if oldInletNames.nonEmpty =>
        val newInletNames = vNewU.info.inputs.map(_.arg)
        inlets.collect {
          case e if oldInletNames.indexOf(e.inlet) < newInletNames.size =>
            e.copy(sourceVertex = vNewU, inletIndex = oldInletNames.indexOf(e.inlet))
        }
      case _ => Vec.empty
    }

    newInlets.foreach(top.addEdge /* .get */)
    vNew match {
      case vu: Vertex.UGen => ChromosomeImpl.completeUGenInputs(top, vu)
      case _ =>
    }
  }

  private def changeEdge(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    val vertices    = top.vertices

    val candidates  = vertices.iterator.collect {
      case v @ Vertex.UGen(spec) if ChromosomeImpl.geArgs(spec).nonEmpty /* spec.inputs.nonEmpty */ => v
    } .toIndexedSeq

    if (candidates.isEmpty) false else {
      val v     = Util.choose(candidates)
      val edges = top.edgeMap.get(v).getOrElse(Set.empty)
      if (edges.isEmpty) false else {
        top.removeEdge(Util.choose(edges))
        ChromosomeImpl.completeUGenInputs(top, v)
        stats(3) += 1
        true
      }
    }
  }

  private def swapEdge(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    val vertices    = top.vertices

    val candidates  = vertices.iterator.collect {
      case v @ Vertex.UGen(spec) if top.edgeMap.get(v).exists(_.size >= 2) => v
    } .toIndexedSeq

    if (candidates.isEmpty) false else {
      val v     = Util.choose(candidates)
      val edges = top.edgeMap.get(v).getOrElse(Set.empty)
      val e1    = Util.choose(edges)
      val e2    = Util.choose(edges - e1)
      top.removeEdge(e1)
      top.removeEdge(e2)
      val e1New = e1.copy(targetVertex = e2.targetVertex)
      val e2New = e2.copy(targetVertex = e1.targetVertex)
      top.addEdge(e1New) // .get
      top.addEdge(e2New) // .get
      stats(4) += 1
      true
    }
  }

  // splits a vertex. candidates are vertices with out degree >= 2.
  // candidate is chosen using roulette algorithm (thus more likely,
  // the higher the out degree).
  private def splitVertex(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    val verticesIn  = top.vertices
    val numVertices = verticesIn.size
    if (numVertices >= Algorithm.maxNumVertices) return false

    val weighted  = verticesIn.iterator.flatMap { v =>
      val set = top.edges.iterator.filter(_.targetVertex == v).toIndexedSeq
      val sz  = set.size
      if (sz > 2) Some(set -> sz) else None
    } .toIndexedSeq
    if (weighted.isEmpty) false else {
      val edges = roulette(weighted)
      // import kollflitz.RandomOps._
      val edgesS: Vec[Edge] = Util.scramble(edges.toVector)
      val (_, edgesMove) = edgesS.splitAt(edgesS.size/2)
      val vertexOld = edges.head.targetVertex
      val vertexNew = vertexOld.copy()
      edgesMove.foreach(top.removeEdge)
      top.addVertex(vertexNew)
      edgesMove.foreach { eOld =>
        val eNew = eOld.copy(targetVertex = vertexNew)
        top.addEdge(eNew) // .get
      }
      top.edgeMap.get(vertexOld).foreach { set =>
        vertexNew match {
          case vNewU: Vertex.UGen =>
            set.foreach { eOld =>
              val eNew = eOld.copy(sourceVertex = vNewU)
              top.addEdge(eNew) // .get
            }
          case _ =>
        }
      }
      checkComplete(top, s"splitVertex()")
      stats(5) += 1
      true
    }
  }

  // merges two vertices of the same type
  private def mergeVertex(top: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    val verticesIn  = top.vertices.iterator.toIndexedSeq
    val numVertices = verticesIn.size
    if (numVertices <= Algorithm.minNumVertices) return false

    // import kollflitz.RandomOps._
    val it = Util.scramble(verticesIn).tails.flatMap {
      case head +: tail =>
        val partners = head match {
          case Vertex.Constant(_) => tail.filter {
            case Vertex.Constant(_) => true // any two constants will do
            case _ => false
          }
          case Vertex.UGen(info) => tail.filter {
            case Vertex.UGen(i1) => i1.name == info.name
            case _ => false
          }
        }
        partners.map(head -> _)

      case _ => None
    }
    if (it.isEmpty) false else {
      val (v1, v2)  = it.next()
      // edges, where v2 is the input
      val edgesOld  = top.edges.iterator.filter(_.targetVertex == v2).toIndexedSeq
      edgesOld.foreach(top.removeEdge)
      if (Util.coin(0.5)) top.removeVertex(v2)

      val check = edgesOld.flatMap { eOld =>
        val eNew = eOld.copy(targetVertex = v1)
        val _ok = top.canAddEdge(eNew)
        if (_ok) {
          top.addEdge(eNew)
          None
        } else {
          Some(eOld.sourceVertex)
        }
      }
      if (check.nonEmpty) {
        check.foreach(ChromosomeImpl.completeUGenInputs(top, _))
      }

      checkComplete(top, s"mergeVertex()")

      stats(6) += 1
      true
    }
  }

  private def checkComplete(succ: Chromosome, message: => String)(implicit tx: S#Tx): Unit =
    succ.vertices.iterator.foreach {
      case v: Vertex.UGen =>
        val inc = ChromosomeImpl.findIncompleteUGenInputs(succ, v)
        if (inc.nonEmpty) {
          println("MISSING SLOTS:")
          inc.foreach(println)
          sys.error(s"UGen is not complete: $v - $message")
        }
      case _ =>
    }

  /*
    ways to mutate:

    - add, remove or alter edges


    - add, remove or alter vertices
    - constant vertex: change value
    - ugen     vertex: exchange?

   */
}