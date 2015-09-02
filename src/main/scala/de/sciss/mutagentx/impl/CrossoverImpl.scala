package de.sciss.mutagentx
package impl

import de.sciss.lucre.data
import de.sciss.lucre.stm.Sys
import de.sciss.mutagentx.Util.coin

import scala.annotation.tailrec

object CrossoverImpl {
  val DEBUG = false

  def apply[S <: Sys[S]](algorithm: Algorithm[S], chosen1: Chromosome[S], chosen2: Chromosome[S])
                        (implicit tx: S#Tx, ord: data.Ordering[S#Tx, Vertex[S]]): Unit = {
    import algorithm.global.{rng => random}
//    var res = Vector.empty[(S#Acc, stm.Source[S#Tx, Chromosome[S]] /* confluent.Source[S, Chromosome[S]] */)]
//    while (res.size < n) {
//      val idx0      = res.size << 1
//      val chosen0H  = sq( idx0      % sq.size)
//      val chosen1H  = sq((idx0 + 1) % sq.size)

//      val csr       = algorithm.global.forkCursor
//      val hs = csr.stepFrom(inputAccess) { implicit tx =>
//        implicit val dtx = tx.durable
//        val chosen1 = chosen0H()
//        val chosen2 = chosen1H()
        val v1      = chosen1.vertices
        val v2      = chosen2.vertices

        // val OLDNUM1 = v1.size
        // val OLDNUM2 = v2.size

        val (pos1, pos2) = if (coin(0.8)) {   // XXX TODO -- make that a parameter
          val posRel  = random.nextFloat()
          val _pos1   = (posRel * v1.size - 1).toInt + 1
          val _pos2   = (posRel * v2.size - 1).toInt + 1
          (_pos1, _pos2)
        } else {
          val posRel1 = random.nextFloat()
          val _pos1   = (posRel1 * v1.size - 1).toInt + 1
          val posRel2 = random.nextFloat()
          val _pos2   = (posRel2 * v2.size - 1).toInt + 1
          (_pos1, _pos2)
        }

        val (head1, tail1)  = v1.iterator.toIndexedSeq.splitAt(pos1)
        val (head2, tail2)  = v2.iterator.toIndexedSeq.splitAt(pos2)
        val edges1          = chosen1.edges.iterator.toSet
        val edges2          = chosen2.edges.iterator.toSet
        val edgesHead1      = edges1.filter(e => head1.contains(e.sourceVertex) && head1.contains(e.targetVertex))
        val edgesTail1      = edges1.filter(e => tail1.contains(e.sourceVertex) && tail1.contains(e.targetVertex))
        val edgesHead2      = edges2.filter(e => head2.contains(e.sourceVertex) && head2.contains(e.targetVertex))
        val edgesTail2      = edges2.filter(e => tail2.contains(e.sourceVertex) && tail2.contains(e.targetVertex))

        val severedHeads1   = edges1.collect {
          case Edge(source: Vertex.UGen[S], target, _) if head1.contains(source) && tail1.contains(target) => source
        }
        val severedHeads2   = edges2.collect {
          case Edge(source: Vertex.UGen[S], target, _) if head2.contains(source) && tail2.contains(target) => source
        }

        @tailrec def shrinkTop(top: Chromosome[S], target: Int, iter: Int): Unit =
          if (top.vertices.size > target && iter != Algorithm.maxNumVertices) {
            MutationImpl.removeVertex1[S](top)
            shrinkTop(top, target = target, iter = iter + 1)
          }

        def mkTop(vertices1: Vec[Vertex[S]], edges1: Set[Edge[S]], vertices2: Vec[Vertex[S]],
                  edges2: Set[Edge[S]]): Chromosome[S] = {
          val c = Chromosome.empty[S] // , Vertex, Edge]
          vertices1.foreach(c.addVertex)
          edges1   .foreach(c.addEdge /* .get */)  // this is now the first half of the original top
          vertices2.foreach(c.addVertex)
          edges2   .foreach(c.addEdge /* .get */)
          c
        }

        val c1 = mkTop(head1, edgesHead1, tail2, edgesTail2)
        val c2 = mkTop(head2, edgesHead2, tail1, edgesTail1)

        def complete(top: Chromosome[S], inc: Set[Vertex.UGen[S]]): Unit = {
          inc.foreach { v => ChromosomeImpl.completeUGenInputs[S](top, v) }
          shrinkTop(chosen1, top.vertices.size, 0)
        }

        complete(c1, severedHeads1)
        complete(c2, severedHeads2)

        // println(s"BEFORE CROSS: $OLDNUM1/$OLDNUM2 AFTER ${c1.vertices.size}/${c2.vertices.size}")

        if (DEBUG) {
          val s1 = s"p1 = (${v1.size}, ${chosen1.edges.size}), p2 = (${v2.size}, ${chosen2.edges.size})"
          val s2 = s"c1 = (${c1.vertices.size}, ${c1.edges.size}), c2 = (${c2.vertices.size}, ${c2.edges.size})"
          println(s"crossover. $s1. $s2")
        }

//        val _res0 = tx.newHandleM(chosen1)
//        val _res1 = if (res.size + 1 == n) _res0 :: Nil else {
//          _res0 :: tx.newHandleM(chosen2) :: Nil
//        }
//
//        _res1
//        // Vector(c1, c2)
//      }
//
//      val pos = csr.step { implicit tx => implicit val dtx = tx.durable; csr.position }
//      if (DEBUG) println(s"$hs - $pos")
//      hs.foreach(h => res :+= (pos, h))
//    }
//
//    res
  }
}