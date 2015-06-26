package de.sciss.mutagentx

trait Graph {
  def vertices    : Vec[Int]
  def inVertices  : Vec[Vec[Int]]
  def outVertices : Vec[Vec[Int]]
}

/**
 * Created by Hanns Holger Rutz on 26/06/15.
 * 
 * Scala translation from Java code, originally
 * created by Dulanga Sashika on 8/26/2014.
 */
object NikolicSim {
  var DEBUG = false

  def apply(graphA: Graph, graphB: Graph, epsilon: Double): Double = {
    val inNodeListA       = graphA.inVertices
    val outNodeListA      = graphA.outVertices
    val inNodeListB       = graphB.inVertices
    val outNodeListB      = graphB.outVertices

    val graphSizeA        = graphA.vertices.size
    val graphSizeB        = graphB.vertices.size

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
          if (DEBUG) print(f"${inNodeSimilarity(i)(j)}%1.3f ")
          nodeSimilarity(i)(j) = (inNodeSimilarity(i)(j) + outNodeSimilarity(i)(j)) / 2
        }
        if (DEBUG) println()
      }
      if (DEBUG) {
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
            val temp = (inNodeSimilarity(i)(j) + outNodeSimilarity(i)(j)) / 2
            if (math.abs(nodeSimilarity(i)(j) - temp) > maxDifference) {
              maxDifference = math.abs(nodeSimilarity(i)(j) - temp)
            }
            nodeSimilarity(i)(j) = temp
          }
        }
      }

      if (DEBUG) {
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

      neighborListMin.foreach { node =>
        var max = 0.0
        var maxIndex = -1
        neighborListMax.foreach { key =>
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
      enumerationFunction(graphA.vertices, graphB.vertices, useGraphA = true ) / graphSizeA
    } else {
      enumerationFunction(graphB.vertices, graphA.vertices, useGraphA = false) / graphSizeB
    }
  }
}