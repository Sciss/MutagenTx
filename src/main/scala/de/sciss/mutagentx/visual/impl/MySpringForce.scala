package de.sciss.mutagentx
package visual
package impl

import prefuse.util.force.{ForceItem, Spring, SpringForce}

object MySpringForce {
  private final val pi    = math.Pi.toFloat
  private final val piH   = (math.Pi/2).toFloat
  private final val twoPi = (math.Pi*2).toFloat
  private final val eps   = (1 * math.Pi/180).toFloat
}
class MySpringForce extends SpringForce {
  import MySpringForce._

  override def getForce(s: Spring): Unit = {
    val item1   = s.item1
    val item2   = s.item2
    val length  = if (s.length < 0) params(SpringForce.SPRING_LENGTH) else s.length
    val x1      = item1.location(0)
    val y1      = item1.location(1)
    val x2      = item2.location(0)
    val y2      = item2.location(1)
    var dx      = x2 - x1
    var dy      = y2 - y1
    val r0      = math.sqrt(dx * dx + dy * dy).toFloat
    val r       = if (r0 == 0.0) {
      dx  = (math.random.toFloat - 0.5f) / 50.0f
      dy  = (math.random.toFloat - 0.5f) / 50.0f
      math.sqrt(dx * dx + dy * dy).toFloat
    } else r0

    val d       = r - length
    val coeff   = (if (s.coeff < 0) params(SpringForce.SPRING_COEFF) else s.coeff) * d / r
    item1.force(0) +=  coeff * dx
    item1.force(1) +=  coeff * dy
    item2.force(0) += -coeff * dx
    item2.force(1) += -coeff * dy

    val ang = math.atan2(dy, dx)  // target angle is Pi/2
    val da0 = -piH - ang
    val da  = if (da0 < -twoPi) da0 + twoPi else da0
    if (math.abs(da) > eps) {
      val cx  = (x1 + x2) / 2
      val cy  = (y1 + y2) / 2
      val af  = da / pi * 0.00005f  // XXX TODO - find that value
      val rH  = r/2
      val cos = math.cos(ang + af).toFloat * rH
      val sin = math.sin(ang + af).toFloat * rH
      val x1t = cx - cos
      val y1t = cy - sin
      val x2t = cx + cos
      val y2t = cy + sin

      item1.force(0) += x1t - x1
      item1.force(1) += y1t - y1
      item2.force(0) += x2t - x2
      item2.force(1) += y2t - y2
    }
  }
}
