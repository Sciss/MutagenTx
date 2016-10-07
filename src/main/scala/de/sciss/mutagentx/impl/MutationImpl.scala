/*
 *  MutationImpl.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.{Obj, Sys}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}

object MutationImpl {
  private val stats = Array.fill(7)(0)

  def printStats(): Unit = println(stats.mkString(", "))

  private def getTargets[S <: Sys[S]](top: Chromosome[S], v: Vertex[S])(implicit tx: S#Tx): Set[Edge[S]] =
    top.sources(v)

//    top.edges.iterator.collect {
//      case e @ Edge(_, `v`, _) => e // a vertex `vi` that uses the removed vertex as one of its inlets
//    } .toSet

  def removeVertex1[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
                                (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Vertex[S] = {
    val vertices    = top.vertices
    val numVertices = vertices.size
    val idx         = random.nextInt(numVertices)
    val v           = vertices(idx)
    removeVertex2(config, top, v)
    v
  }

  def removeVertex2[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S], v: Vertex[S])
                                (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Unit = {
    val targets = getTargets(top, v)
    top.removeVertex(v)
//    targets.foreach { e =>
//      top.removeEdge(e)
//    }
    targets.foreach { case Edge(t: Vertex.UGen[S], _, _) =>
      assert(t != v)
      ChromosomeImpl.completeUGenInputs[S](config, top, t)
    }
  }

  /** Produces a sequence of `n` items by mutating the input `sq` selection.
    *
    * It assumes the invoking transaction is 'up-to-date' and will cause
    * the selection's cursors to step from this transaction's input access.
    */
  def apply[S <: Sys[S]](algorithm: Algorithm[S], chosen: Chromosome[S])(implicit tx: S#Tx): Boolean = {
    // var res = Vector.empty[(S#Acc, stm.Source[S#Tx, Chromosome[S]] /* confluent.Source[S, Chromosome[S]] */)]

    import algorithm.config
    import config.breeding.{mutMin, mutMax}

    // while (res.size < n) {
      // val chosenH = sq(res.size % sq.size)
      // val csr     = algorithm.global.forkCursor
      // val hOpt    = csr.stepFrom(inputAccess) { implicit tx =>
        import algorithm.global.{rng => random}
        // val chosen        = chosenH()
        val mutationIter  = Util.rrand(mutMin, mutMax)
        require(mutationIter > 0)
        val success       = (false /: (1 to mutationIter)) { case (success0, iter) =>
          val tpe = random.nextInt(7)
          // val OLDNUM = chosen.vertices.size
          val success1 = (tpe: @switch) match {
            case 0 => addVertex   (config, chosen)
            case 1 => removeVertex(config, chosen)
            case 2 => changeVertex(config, chosen)
            case 3 => changeEdge  (config, chosen)
            case 4 => swapEdge    (chosen)
            case 5 => splitVertex (config, chosen)
            case 6 => mergeVertex (config, chosen)
          }
          // println(s"BEFORE $tpe ($success1): $OLDNUM AFTER ${chosen.vertices.size}")

          if (success1) {
            chosen.validate1()
          }

          success0 | success1 // i.e. at least one mutation was applied
        }
        success // if (success) Some(tx.newHandleM(chosen)) else None
//      }
//      hOpt.foreach { h =>
//        val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
//        if (Algorithm.DEBUG) println(s"$h - $pos")
//        res :+= (pos, h)
//      }
//    }
//    res
  }

  private def roulette[S <: Sys[S], A](in: Vec[(A, Int)])(implicit tx: S#Tx, random: TxnRandom[S#Tx]): A = {
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

  private def addVertex[S <: Sys[S]](config: Algorithm.Config, c: Chromosome[S])
                                    (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    import config.generation._
    val numVertices = c.vertices.size
    if (numVertices >= maxNumVertices) false else {
      ChromosomeImpl.addVertex[S](config, c)
      checkComplete(c, s"addVertex()")
      stats(0) += 1
      true
    }
  }

  private def removeVertex[S <: Sys[S]](config: Algorithm.Config, c: Chromosome[S])
                                       (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    import config.generation._
    val vertices    = c.vertices
    val numVertices = vertices.size
    if (numVertices <= minNumVertices) false else {
      removeVertex1(config, c)
      checkComplete(c, s"removeVertex($c)")
      stats(1) += 1
      true
    }
  }

  private def changeVertex[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
                                       (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    val vertices    = top.vertices
    val numVertices = vertices.size

    val idx     = random.nextInt(numVertices)
    val vOld    = vertices(idx)
    vOld match {
      case f: Vertex.Constant[S] => changeVertexConstant(        top, f)
      case u: Vertex.UGen[S]     => changeVertexUGen    (config, top, u)
    }
    stats(2) += 1

    true
  }

  private def changeVertexConstant[S <: Sys[S]](top: Chromosome[S], vc: Vertex.Constant[S])
                                               (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Unit = {
    val fNew: Float = if (Util.coin())
      ChromosomeImpl.mkConstantValue()            // completely random
    else
      vc.f * Util.exprand(0.9, 1.0/0.9).toFloat  // gradual change

    vc.f = fNew
  }

  private def changeVertexUGen[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S], vu: Vertex.UGen[S])
                                           (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Unit = {
    val outlet  = getTargets(top, vu)
    val inlets  = top.targets(vu) // .getOrElse(Set.empty)
    outlet.foreach(top.removeEdge)
    inlets.foreach(top.removeEdge)
    top.removeVertex(vu)

    val vNew = ChromosomeImpl.mkUGen()

    val oldInletNames: Vec[String] = vu match {
      case Vertex.UGen(info) => /* ChromosomeImpl.geArgs(info).map(_.name) */ info.inputs.map(_.arg)
      case _ => Vec.empty
    }

    top.addVertex(vNew)
    outlet.map(_.copy1(targetVertex = vNew)).foreach(top.addEdge /* .get */)

    // just as many as possible, leaving tail inlets empty
    val newInlets = vNew match {
      case vNewU: Vertex.UGen[S] if oldInletNames.nonEmpty =>
        val newInletNames = vNewU.info.inputs.map(_.arg)
        inlets.collect {
          case e if oldInletNames.indexOf(e.inlet) < newInletNames.size =>
            e.copy1(sourceVertex = vNewU, inletIndex = oldInletNames.indexOf(e.inlet))
        }
      case _ => Vec.empty
    }

    newInlets.foreach(top.addEdge /* .get */)
    vNew match {
      case vu: Vertex.UGen[S] => ChromosomeImpl.completeUGenInputs(config, top, vu)
      case _ =>
    }
  }

  private def changeEdge[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
                                     (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    val vertices    = top.vertices

    val candidates  = vertices.iterator.collect {
      case v: Vertex.UGen[S] if ChromosomeImpl.geArgs(v.info).nonEmpty /* spec.inputs.nonEmpty */ => v
    } .toIndexedSeq

    if (candidates.isEmpty) false else {
      val v     = Util.choose(candidates)
      val edges = top.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
      if (edges.isEmpty) false else {
        top.removeEdge(Util.choose(edges))
        ChromosomeImpl.completeUGenInputs[S](config, top, v)
        stats(3) += 1
        true
      }
    }
  }

  private def swapEdge[S <: Sys[S]](top: Chromosome[S])(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    val vertices    = top.vertices

    val candidates  = vertices.iterator.collect {
      case v: Vertex.UGen[S] if top.targets(v).size >= 2 /* edgeMap.get(v).exists(_.size >= 2) */ => v
    } .toIndexedSeq

    if (candidates.isEmpty) false else {
      val v     = Util.choose(candidates)
      val edges = top.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
      val e1    = Util.choose(edges)
      val e2    = Util.choose(edges - e1)
      top.removeEdge(e1)
      top.removeEdge(e2)
      val e1New = e1.copy1(targetVertex = e2.targetVertex)
      val e2New = e2.copy1(targetVertex = e1.targetVertex)
      top.addEdge(e1New) // .get
      top.addEdge(e2New) // .get
      stats(4) += 1
      true
    }
  }

  // splits a vertex. candidates are vertices with out degree >= 2.
  // candidate is chosen using roulette algorithm (thus more likely,
  // the higher the out degree).
  private def splitVertex[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
                                      (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    import config.generation._
    val verticesIn  = top.vertices
    val numVertices = verticesIn.size
    if (numVertices >= maxNumVertices) return false

    val weighted  = verticesIn.iterator.flatMap { v =>
      val set = top.edges.iterator.filter(_.targetVertex == v).toIndexedSeq
      val sz  = set.size
      if (sz > 2) Some(set -> sz) else None
    } .toIndexedSeq
    if (weighted.isEmpty) false else {
      val edges = roulette(weighted)
      // import kollflitz.RandomOps._
      val edgesS: Vec[Edge[S]] = Util.scramble(edges.toVector)
      val (_, edgesMove) = edgesS.splitAt(edgesS.size/2)
      val vertexOld = edges.head.targetVertex
      val vertexNew = Obj.copy(vertexOld) // .copy1()
      edgesMove.foreach(top.removeEdge)
      top.addVertex(vertexNew)
      edgesMove.foreach { eOld =>
        val eNew = eOld.copy1(targetVertex = vertexNew)
        top.addEdge(eNew) // .get
      }
      val set = top.targets(vertexOld) /* edgeMap.get(vertexOld).foreach { set => */
        vertexNew match {
          case vNewU: Vertex.UGen[S] =>
            set.foreach { eOld =>
              val eNew = eOld.copy1(sourceVertex = vNewU)
              top.addEdge(eNew) // .get
            }
          case _ =>
        }
      // }
      checkComplete(top, s"splitVertex()")
      stats(5) += 1
      true
    }
  }

  // merges two vertices of the same type
  private def mergeVertex[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
                                      (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    import config.generation._
    val verticesIn  = top.vertices.iterator.toIndexedSeq
    val numVertices = verticesIn.size
    if (numVertices <= minNumVertices) return false

    // import kollflitz.RandomOps._
    val it = Util.scramble(verticesIn).tails.flatMap {
      case head +: tail =>
        val partners = head match {
          case _: Vertex.Constant[S] => tail.filter {
            case _: Vertex.Constant[S] => true // any two constants will do
            case _ => false
          }
          case v: Vertex.UGen[S] => tail.filter {
            case v1: Vertex.UGen[S] => v1.info.name == v.info.name
            case _ => false
          }
        }
        partners.map(head -> _)

      case _ => None
    }
    if (it.isEmpty) false else {
      val (v1, v2)  = it.next()
      // edges, where v2 is the input
      val edgesOld  = top.sources(v2) // edges.iterator.filter(_.targetVertex == v2).toIndexedSeq
      edgesOld.foreach(top.removeEdge)
      if (Util.coin(0.5)) top.removeVertex(v2)

      val check = edgesOld.flatMap { eOld =>
        val eNew = eOld.copy1(targetVertex = v1)
        val _ok = top.canAddEdge(eNew)
        if (_ok) {
          top.addEdge(eNew)
          None
        } else {
          Some(eOld.sourceVertex)
        }
      }
      if (check.nonEmpty) {
        check.foreach(ChromosomeImpl.completeUGenInputs(config, top, _))
      }

      checkComplete(top, "mergeVertex()")

      stats(6) += 1
      true
    }
  }

  private[this] final val CHECK = false

  private def checkComplete[S <: Sys[S]](succ: Chromosome[S], message: => String)(implicit tx: S#Tx): Unit =
    if (CHECK) succ.vertices.iterator.foreach {
      case v: Vertex.UGen[S] =>
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