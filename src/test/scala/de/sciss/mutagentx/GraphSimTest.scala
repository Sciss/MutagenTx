package de.sciss.mutagentx

object GraphSimTest extends App {
  val g = NikolicSimilarity.Graph[String]
  val h = NikolicSimilarity.Graph[String]
  g.addVertex("A")
  g.addVertex("B")
  g.addVertex("C")

  val s1 = NikolicSimilarity(g, g, epsilon = 0.01)
  println(s"Similarity of unconnected g with itself: $s1")  // 1.0

  val s2 = NikolicSimilarity(g, h, epsilon = 0.01)
  println(s"Similarity of g with empty h: $s2")   // 0.0

  val s3 = NikolicSimilarity(h, h, epsilon = 0.01)
  println(s"Similarity of empty h with itself: $s3")  // 1.0

  h.addVertex("C")
  h.addVertex("A")

  val s4 = NikolicSimilarity(g, h, epsilon = 0.01)
  println(s"Similarity of (A,B,C) with (C,A): $s4")   // 1.0 --- XXX TODO is this correct?

  g.addEdge("A", "B")
  val s5 = NikolicSimilarity(g, h, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (C,A): $s5")   // 0.75

  h.addEdge("A", "C")
  val s6 = NikolicSimilarity(g, h, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (C -> A): $s6") // 0.5

  val j = NikolicSimilarity.Graph[String]
  j.addVertex("D")
  j.addVertex("E")
  j.addVertex("F")
  j.addEdge("F", "E")
  j.addEdge("D", "E")

  val s7 = NikolicSimilarity(g, j, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (D -> E, F -> E): $s7") // 0.0
}