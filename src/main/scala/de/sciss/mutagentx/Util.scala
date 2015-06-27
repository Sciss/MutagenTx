package de.sciss.mutagentx

import java.awt.{Font, Color}

import de.sciss.desktop.Desktop
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm.Sys
import de.sciss.numbers
import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import org.jfree.chart.renderer.xy.{XYStepAreaRenderer, StandardXYBarPainter, XYBarRenderer}

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalax.chart.module.Charting
import scalax.chart.{XYChart, Chart}

object Util {
  // ---- random functions ----
  // cf. https://github.com/Sciss/Dissemination/blob/master/src/main/scala/de/sciss/semi/Util.scala

  def rrand[S <: Sys[S]](lo: Int, hi: Int)(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Int =
    lo + random.nextInt(hi - lo + 1)

  def exprand[S <: Sys[S]](lo: Double, hi: Double)(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

  def coin[S <: Sys[S]](p: Double = 0.5)(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean =
    random.nextDouble() < p

  def choose[S <: Sys[S], A](xs: Iterable[A])(implicit tx: S#Tx, random: TxnRandom[S#Tx]): A =
    xs.toIndexedSeq(random.nextInt(xs.size))

  def scramble[S <: Sys[S], A, CC[~] <: IndexedSeq[~], To](in: CC[A])(implicit tx: S#Tx, random: TxnRandom[S#Tx],
                                                         cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: IndexedSeq[A]
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

  // ---- plotting ----

  private val defaultFontFace = if (Desktop.isLinux) "Liberation Sans" else if (Desktop.isWindows) "Arial" else "Helvetica"

  def mkNiceChart(chart: Chart): Unit = {
    val plot = chart.plot

    val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
      case p: XYPlot       =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
      case p: CategoryPlot =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
    }

    val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
    val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
    xAxis.setLabelFont(fnt1)
    xAxis.setTickLabelFont(fnt2)
    yAxis.setLabelFont(fnt1)
    yAxis.setTickLabelFont(fnt2)
  }

  def mkHistogramChart(histo: Vec[Double], xMin: Double, xMax: Double, title: String): XYChart = {
    import numbers.Implicits._
    import Charting._
    val data: Vec[(Double, Double)] = histo.zipWithIndex.map { case (num, i) =>
      (i + 0.5).linlin(0, histo.length, xMin, xMax) -> num
    } (breakOut)
    val dataCol = data.toXYSeriesCollection(title)
    val chart   = XYLineChart(dataCol, title = title, legend = false)
    mkNiceChart(chart)
    val plot    = chart.plot
    val renderer = new XYStepAreaRenderer()
    plot.setRenderer(renderer)
    renderer.setSeriesPaint(0, Color.darkGray)
    chart
  }
}