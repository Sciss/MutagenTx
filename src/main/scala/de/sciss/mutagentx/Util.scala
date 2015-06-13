package de.sciss.mutagentx

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.synth.SynthGraph

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

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

  def scramble[A, CC[~] <: IndexedSeq[~], To](in: CC[A])(implicit tx: S#Tx, random: TxnRandom[D#Tx],
                                                         cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: IndexedSeq[A]
    implicit val dtx = tx.durable
    while (rem.nonEmpty) {
      val idx = random.nextInt(rem.size)
      val e = rem(idx)
      rem = rem.patch(idx, Nil, 1)
      b += e
    }
    b.result()
  }

  // ---- signal processing functions ----

  /** Mutates `a` by multiplying its contents with `b`. */
  def mul(a: Array[Float], aOff: Int, b: Array[Float], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) *= b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by adding `b` to it. */
  def add(a: Array[Double], aOff: Int, b: Array[Double], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) += b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by multiplying each element with `f` */
  def mul(a: Array[Double], off: Int, len: Int, f: Double): Unit = {
    var ai = off
    val stop = ai + len
    while (ai < stop) {
      a(ai) *= f
      ai += 1
    }
  }

  /** Calculates RMS */
  def energy(in: Array[Float], off: Int, len: Int): Double = {
    var sum = 0.0
    var i = off
    val j = i + len
    while (i < j) {
      sum += in(i) * in(i)
      i += 1
    }
    math.sqrt(sum / len)
  }

  /** Discrete cosine transform. */
  def dct(in: Array[Double], off: Int, len: Int, numCoeff: Int): Array[Double] = {
    val c = new Array[Double](numCoeff)
    var n = 1
    val r = math.Pi / len
    while (n <= numCoeff) {
      var i = 1
      val s = r * (n - 1)
      while (i <= len) {
        c(n - 1) += in(i + off - 1) * math.cos(s * (i - 0.5))
        i += 1
      }
      n += 1
    }
    c
  }
}