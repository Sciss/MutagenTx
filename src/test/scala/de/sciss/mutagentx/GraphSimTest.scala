package de.sciss.mutagentx

object GraphSimTest extends App {
  val g = NikolicSim.Graph[String]
  val h = NikolicSim.Graph[String]
  g.addVertex("A")
  g.addVertex("B")
  g.addVertex("C")

  val s1 = NikolicSim(g, g, epsilon = 0.01)
  println(s"Similarity of unconnected g with itself: $s1")  // 1.0

  val s2 = NikolicSim(g, h, epsilon = 0.01)
  println(s"Similarity of g with empty h: $s2")   // 0.0

  val s3 = NikolicSim(h, h, epsilon = 0.01)
  println(s"Similarity of empty h with itself: $s3")  // 1.0

  h.addVertex("C")
  h.addVertex("A")

  val s4 = NikolicSim(g, h, epsilon = 0.01)
  println(s"Similarity of (A,B,C) with (C,A): $s4")   // 1.0 --- XXX TODO is this correct?

  g.addEdge("A", "B")
  val s5 = NikolicSim(g, h, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (C,A): $s5")   // 0.75

  h.addEdge("A", "C")
  val s6 = NikolicSim(g, h, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (C -> A): $s6") // 0.5

  val j = NikolicSim.Graph[String]
  j.addVertex("D")
  j.addVertex("E")
  j.addVertex("F")
  j.addEdge("F", "E")
  j.addEdge("D", "E")

  val s7 = NikolicSim(g, j, epsilon = 0.01)
  println(s"Similarity of (A -> B,C) with (D -> E, F -> E): $s7") // 0.0
}