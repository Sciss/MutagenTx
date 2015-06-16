package de.sciss.mutagentx.impl

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.event.Sys

object TxnRandomBridge {
  def apply[S <: Sys[S], D <: Sys[D]](peer: TxnRandom[D#Tx])(implicit bridge: S#Tx => D#Tx): TxnRandom[S#Tx] =
    new Impl[S, D](peer, bridge)

  private final class Impl[S <: Sys[S], D <: Sys[D]](peer: TxnRandom[D#Tx], bridge: S#Tx => D#Tx)
    extends TxnRandom[S#Tx] {

    def setSeed(seed: Long)(implicit tx: S#Tx): Unit = peer.setSeed(seed)(bridge(tx))

    def nextInt    ()(implicit tx: S#Tx): Int     = peer.nextInt    ()(bridge(tx))
    def nextLong   ()(implicit tx: S#Tx): Long    = peer.nextLong   ()(bridge(tx))
    def nextFloat  ()(implicit tx: S#Tx): Float   = peer.nextFloat  ()(bridge(tx))
    def nextDouble ()(implicit tx: S#Tx): Double  = peer.nextDouble ()(bridge(tx))
    def nextBoolean()(implicit tx: S#Tx): Boolean = peer.nextBoolean()(bridge(tx))

    def nextInt(n: Int)(implicit tx: S#Tx): Int   = peer.nextInt(n)(bridge(tx))
  }
}
