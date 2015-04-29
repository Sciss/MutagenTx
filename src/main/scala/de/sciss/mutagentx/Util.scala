package de.sciss.mutagentx

import de.sciss.lucre.confluent.TxnRandom

object Util {
  // ---- random functions ----
  // cf. https://github.com/Sciss/Dissemination/blob/master/src/main/scala/de/sciss/semi/Util.scala

  def rrand  (lo: Int   , hi: Int   )(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Int    =
    lo + random.nextInt(hi - lo + 1)(tx.durable)

  def exprand(lo: Double, hi: Double)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble()(tx.durable))

  def coin(p: Double = 0.5)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean =
    random.nextDouble()(tx.durable) < p

  def choose[A](xs: Iterable[A])(implicit tx: S#Tx, random: TxnRandom[D#Tx]): A =
    xs.toIndexedSeq(random.nextInt(xs.size)(tx.durable))
}
