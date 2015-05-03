package de.sciss.mutagentx
package impl

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.mutagentx.Util.coin

import scala.annotation.tailrec

object CrossoverImpl {
  val DEBUG = false

  def apply(gs: Vec[Chromosome], sz: Int)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Vec[Chromosome] = {
    implicit val dtx = tx.durable
    // - choose two parents
    // - choose relative cutting point; determine absolute point in both parents
    // - create two child chromosomes from split parents
    // - fix them by completing missing links
    // - for now: ignore min/max vertices
    val szH     = (sz + 1) / 2
    val res     = (0 until szH).flatMap { i =>
      // val pIdx1   = rnd.nextInt(genome.size)
      // val pIdx2a  = rnd.nextInt(genome.size - 1)
      // val pIdx2   = if (pIdx2a < pIdx1) pIdx2a else pIdx2a + 1  // so pIdx1 != pIdx2
      val p1      = gs( (i << 1)      % gs.size) // genome(pIdx1)
      val p2      = gs(((i << 1) + 1) % gs.size) // genome(pIdx2)

      val top1    = p1 // .top
      val top2    = p2 // .top
      val v1      = top1.vertices
      val v2      = top2.vertices

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
      val edges1          = top1.edges.iterator.toSet
      val edges2          = top2.edges.iterator.toSet
      val edgesHead1      = edges1.filter(e => head1.contains(e.sourceVertex) && head1.contains(e.targetVertex))
      val edgesTail1      = edges1.filter(e => tail1.contains(e.sourceVertex) && tail1.contains(e.targetVertex))
      val edgesHead2      = edges2.filter(e => head2.contains(e.sourceVertex) && head2.contains(e.targetVertex))
      val edgesTail2      = edges2.filter(e => tail2.contains(e.sourceVertex) && tail2.contains(e.targetVertex))

      val severedHeads1   = edges1.collect {
        case Edge(source: Vertex.UGen, target, _) if head1.contains(source) && tail1.contains(target) => source
      }
      val severedHeads2   = edges2.collect {
        case Edge(source: Vertex.UGen, target, _) if head2.contains(source) && tail2.contains(target) => source
      }

      @tailrec def shrinkTop(top: Chromosome, target: Int, iter: Int): Unit =
        if (top.vertices.size > target && iter != Algorithm.maxNumVertices) {
          MutationImpl.removeVertex1(top)
          shrinkTop(top, target = target, iter = iter + 1)
        }

      def mkTop(vertices1: Vec[Vertex], edges1: Set[Edge], vertices2: Vec[Vertex], edges2: Set[Edge]): Chromosome = {
        val c = Topology.empty[Vertex, Edge]
        vertices1.foreach(c.addVertex)
        edges1   .foreach(c.addEdge(_).get)  // this is now the first half of the original top
        vertices2.foreach(c.addVertex)
        edges2   .foreach(c.addEdge(_).get)
        c
      }

      val c1 = mkTop(head1, edgesHead1, tail2, edgesTail2)
      val c2 = mkTop(head2, edgesHead2, tail1, edgesTail1)

      def complete(top: Chromosome, inc: Set[Vertex.UGen]): Unit = {
        inc.foreach { v => ChromosomeImpl.completeUGenInputs(top, v) }
        shrinkTop(top1, top.vertices.size, 0)
      }

      complete(c1, severedHeads1)
      complete(c2, severedHeads2)

      if (DEBUG) {
        val s1 = s"p1 = (${v1.size}, ${top1.edges.size}), p2 = (${v2.size}, ${top2.edges.size})"
        val s2 = s"c1 = (${c1.vertices.size}, ${c1.edges.size}), c2 = (${c2.vertices.size}, ${c2.edges.size})"
        println(s"crossover. $s1. $s2")
      }

      Vector(c1, c2)
    }

    if (res.size == sz) res else res.take(sz)
  }
}