/*
 *  MySpringForce.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx
package visual
package impl

import prefuse.util.force.{Spring, SpringForce}

object MySpringForce {
  private final val pi    = math.Pi.toFloat
  private final val piH   = (math.Pi/2).toFloat
  private final val eps   = (1 * math.Pi/180).toFloat
}
class MySpringForce extends SpringForce {
  import MySpringForce._

  private val TORQUE    = params.length
  private val DISTANCE  = TORQUE + 1

  params    = params    ++ Array[Float](5e-5f, -1f)
  minValues = minValues ++ Array[Float](0f   , -1f)
  maxValues = maxValues ++ Array[Float](1e-3f, 500f)

  override def getParameterNames: Array[String] =
    super.getParameterNames ++ Array("Torque", "Limit")

  // https://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
  private def angleBetween(a: Float, b: Float): Float = {
    val d = b - a
    math.atan2(math.sin(d), math.cos(d)).toFloat
  }

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
    val r1      = if (r0 == 0.0) {
      dx  = (math.random.toFloat - 0.5f) / 50.0f
      dy  = (math.random.toFloat - 0.5f) / 50.0f
      math.sqrt(dx * dx + dy * dy).toFloat
    } else r0
    val dist    = params(DISTANCE)
    val r       = if (dist < 0) r1 else math.min(dist, r1)

    val d       = r - length
    val coeff   = (if (s.coeff < 0) params(SpringForce.SPRING_COEFF) else s.coeff) * d / r
    item1.force(0) +=  coeff * dx
    item1.force(1) +=  coeff * dy
    item2.force(0) += -coeff * dx
    item2.force(1) += -coeff * dy

    val ang = math.atan2(dy, dx).toFloat
    val da = angleBetween(ang, -piH)
    if (math.abs(da) > eps) {
      val af  = da / pi * params(TORQUE)
      // println(f"ang = $ang%1.3f, da = $da%1.3f, af = $af%1.3f")
      val rH  = r/2
      val cx  = (x1 + x2) / 2
      val cy  = (y1 + y2) / 2
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
