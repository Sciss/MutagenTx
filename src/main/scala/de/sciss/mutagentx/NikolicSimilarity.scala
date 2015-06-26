/*
 *  NikolicSimilarity.scala
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

/** Implementation for algorithm described
  * in Mladen Nikolić, "Measuring Similarity of Graph Nodes by Neighbor Matching"
  *
  * Based on Java code by Dulanga Sashika.
  */
object NikolicSimilarity {
  var DEBUG = false

  object Graph {
    trait Modifiable[A] extends Graph[A] {
      def addVertex(v: A): Unit
      def addEdge(source: A, sink: A): Unit
      def copy(): Modifiable[A]
    }

    def apply[A]: Modifiable[A] = new Impl[A](Vector.empty, Vector.empty, Vector.empty)

    private final class Impl[A](private var _vertices   : Vector[A],
                                private var _inVertices : Vector[Vector[Int]],
                                private var _outVertices: Vector[Vector[Int]])
      extends Modifiable[A] {

      def copy(): Modifiable[A] = new Impl(_vertices, _inVertices = _inVertices, _outVertices = _outVertices)

      def vertices   : Vec[A]         = _vertices
      def inVertices : Vec[Vec[Int]]  = _inVertices
      def outVertices: Vec[Vec[Int]]  = _outVertices

      def addVertex(v: A): Unit = {
        _vertices    :+= v
        _inVertices  :+= Vector.empty
        _outVertices :+= Vector.empty
      }

      def addEdge(source: A, sink: A): Unit = {
        val sourceIdx = _vertices.indexOf(source)
        if (sourceIdx < 0) throw new NoSuchElementException(source.toString)
        val sinkIdx   = _vertices.indexOf(sink  )
        if (sinkIdx   < 0) throw new NoSuchElementException(sink  .toString)

        _inVertices   = _inVertices .updated(sinkIdx  , _inVertices (sinkIdx  ) :+ sourceIdx)
        _outVertices  = _outVertices.updated(sourceIdx, _outVertices(sourceIdx) :+ sinkIdx  )
      }
    }
  }
  trait Graph[+A] {
    def vertices    : Vec[A]
    def inVertices  : Vec[Vec[Int]]
    def outVertices : Vec[Vec[Int]]
  }

  def apply[A](graphA: Graph[A], graphB: Graph[A], epsilon: Double = 0.01): Double = {
    val graphSizeA        = graphA.vertices.size
    val graphSizeB        = graphB.vertices.size

    if (graphSizeA == 0 || graphSizeB == 0) return if (graphSizeA == graphSizeB) 1.0 else 0.0

    val _DEBUG = DEBUG

    val inNodeListA       = graphA.inVertices
    val outNodeListA      = graphA.outVertices
    val inNodeListB       = graphB.inVertices
    val outNodeListB      = graphB.outVertices

    val nodeSimilarity    = Array.ofDim[Double](graphSizeA, graphSizeB)
    val inNodeSimilarity  = Array.ofDim[Double](graphSizeA, graphSizeB)
    val outNodeSimilarity = Array.ofDim[Double](graphSizeA, graphSizeB)

    def init(): Unit = {
      for (i <- 0 until graphSizeA) {
        for (j <- 0 until graphSizeB) {
          val aInSz = inNodeListA(i).size
          val bInSz = inNodeListB(j).size
          val maxInDegree = math.max(aInSz, bInSz)
          inNodeSimilarity(i)(j) = if (maxInDegree != 0) {
            math.min(aInSz, bInSz) / maxInDegree
          } else {
            0.0
          }

          val aOutSz = outNodeListA(i).size
          val bOutSz = outNodeListB(j).size
          val maxOutDegree = math.max(aOutSz, bOutSz)
          outNodeSimilarity(i)(j) = if (maxOutDegree != 0) {
             math.min(aOutSz, bOutSz) / maxOutDegree
          } else {
            0.0
          }
        }
      }

      for (i <- 0 until graphSizeA) {
        for (j <- 0 until graphSizeB) {
          if (_DEBUG) print(f"${inNodeSimilarity(i)(j)}%1.3f ")
          nodeSimilarity(i)(j) = (inNodeSimilarity(i)(j) + outNodeSimilarity(i)(j)) / 2
        }
        if (_DEBUG) println()
      }
      if (_DEBUG) {
        println()
        for (i <- 0 until graphSizeA) {
          for (j <- 0 until graphSizeB) {
            print(f"${outNodeSimilarity(i)(j)}%1.3f ")
          }
          println()
        }
        println()
        for (i <- 0 until graphSizeA) {
          for (j <- 0 until graphSizeB) {
            print(f"${nodeSimilarity(i)(j)}%1.3f ")
          }
          println()
        }
      }
    }

    val getSimA = { (node: Int, key: Int) => nodeSimilarity(node)(key ) }
    val getSimB = { (node: Int, key: Int) => nodeSimilarity(key )(node) }

    def measureSimilarity(): Unit = {
      var maxDifference = epsilon

      while (maxDifference >= epsilon) {
        maxDifference = 0.0
        for (i <- 0 until graphSizeA) {
          for (j <- 0 until graphSizeB) {
            // calculate in-degree similarities
            val aInSz = inNodeListA(i).size
            val bInSz = inNodeListB(j).size
            val maxInDegree = math.max(aInSz, bInSz)
            val minInDegree = math.min(aInSz, bInSz)
            val similarityInSum = if (minInDegree == aInSz) {
              enumerationFunction(inNodeListA(i), inNodeListB(j), useGraphA = true )
            } else {
              enumerationFunction(inNodeListB(j), inNodeListA(i), useGraphA = false)
            }
            inNodeSimilarity(i)(j) = if (maxInDegree == 0) {
              if (similarityInSum == 0.0) 1.0 else 0.0
            } else {
              similarityInSum / maxInDegree
            }

            // calculate out-degree similarities
            val aOutSz = outNodeListA(i).size
            val bOutSz = outNodeListB(j).size
            val maxOutDegree = math.max(aOutSz, bOutSz)
            val minOutDegree = math.min(aOutSz, bOutSz)
            val similarityOutSum = if (minOutDegree == aOutSz) {
              enumerationFunction(outNodeListA(i), outNodeListB(j), useGraphA = true )
            } else {
              enumerationFunction(outNodeListB(j), outNodeListA(i), useGraphA = false)
            }
            outNodeSimilarity(i)(j) = if (maxOutDegree == 0) {
              if (similarityOutSum == 0.0) 1.0 else 0.0
            } else {
              similarityOutSum / maxOutDegree
            }
          }
        }

        for (i <- 0 until graphSizeA) {
          for (j <- 0 until graphSizeB) {
            // XXX TODO -- here we could insert a more refined comparison
            nodeSimilarity(i)(j) = if (graphA.vertices(i) != graphB.vertices(j)) 0.0 else {
              val temp = (inNodeSimilarity(i)(j) + outNodeSimilarity(i)(j)) / 2
              if (math.abs(nodeSimilarity(i)(j) - temp) > maxDifference) {
                maxDifference = math.abs(nodeSimilarity(i)(j) - temp)
              }
              temp
            }
          }
        }
      }

      if (_DEBUG) {
        for (i <- 0 until graphSizeA) {
          for (j <- 0 until graphSizeB) {
            print(f"${nodeSimilarity(i)(j)}%1.3f ")
          }
          println()
        }
      }
    }

    def enumerationFunction(neighborListMin: Vec[Int], neighborListMax: Vec[Int], useGraphA: Boolean): Double = {
      val getSim    = if (useGraphA) getSimA else getSimB
      var valueMap  = Map.empty[Int, Double]

      neighborListMin.iterator.foreach { node =>
        var max = 0.0
        var maxIndex = -1
        neighborListMax.iterator.foreach { key =>
          if (!valueMap.contains(key)) {
            val sim = getSim(node, key)
            if (sim > max) {
              max = sim
              maxIndex = key
            }
          }
        }
        valueMap += maxIndex -> max
      }

      valueMap.valuesIterator.sum
    }

    // ---- run ----

    init()
    measureSimilarity()

    if (graphSizeA < graphSizeB) {
      enumerationFunction(graphA.vertices.indices, graphB.vertices.indices, useGraphA = true ) / graphSizeA
    } else {
      enumerationFunction(graphB.vertices.indices, graphA.vertices.indices, useGraphA = false) / graphSizeB
    }
  }
}