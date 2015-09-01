package de.sciss.mutagentx

import de.sciss.lucre.{confluent, data}
import de.sciss.lucre.stm.Identifiable

import scala.annotation.tailrec

object ConfluentOrdering {
  type S = confluent.Confluent

  implicit def apply[V <: Identifiable[S#ID]]: data.Ordering[S#Tx, V] = anyOrd.asInstanceOf[Ord[V]]

  private val anyOrd = new Ord[Identifiable[S#ID]]

  private final class Ord[V <: Identifiable[S#ID]] extends data.Ordering[S#Tx, V] {
    def compare(a: V, b: V)(implicit tx: S#Tx): Int = {
      val aid = a.id
      val bid = b.id
      val ab  = aid.base
      val bb  = bid.base
      if (ab < bb) -1 else if (ab > bb) 1 else {
        @tailrec def loop(ap: S#Acc,  bp: S#Acc): Int =
          if (ap.isEmpty) {
            -1 // we rule out equality before; if (bp.isEmpty) 0 else -1
          } else if (bp.isEmpty) 1 else {
            val ah = ap.head.toInt
            val bh = bp.head.toInt
            if (ah < bh) -1 else if (ah > bh) 1 else {
              loop(ap.tail, bp.tail)
            }
          }

        if (aid.path == bid.path) 0 else loop(aid.path, bid.path)
      }
    }
  }
}
