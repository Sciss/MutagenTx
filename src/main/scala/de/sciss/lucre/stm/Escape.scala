package de.sciss.lucre.stm

object Escape {
  def inMemoryID(in: InMemoryLike.ID[_]): Int = in.id
}
