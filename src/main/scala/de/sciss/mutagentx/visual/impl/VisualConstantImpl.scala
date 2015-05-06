package de.sciss.mutagentx
package visual
package impl

import java.math.{RoundingMode, MathContext}

import prefuse.visual.VisualItem

import scala.concurrent.stm.Ref
import scala.swing._

object VisualConstantImpl {
  import VisualNodeImpl.diam

  def apply(_main: Visual, v: Vertex.Constant)(implicit tx: S#Tx): VisualConstant = new VisualConstant with VisualVertexImpl {
    private var _value = v.f()
    private var _targetValue = _value

    val main  = _main

    def value: Float = _targetValue
    def value_=(x: Float): Unit = {
      _targetValue = x
    }

    private var _name: String = _

    private def mkName(): Unit = _name = {
      val mathContext = new MathContext(6, RoundingMode.DOWN)
      val bigDecimal = BigDecimal(_value, mathContext)
      bigDecimal.bigDecimal.toPlainString
    }

    mkName()

    val active = Ref(tx.inputAccess.term.toInt)

    override def toString = s"VisualConstant($value)@${hashCode.toHexString}"

    protected def boundsResized(): Unit = ()

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit = {
      _value = 0.9f * _value + 0.1f * _targetValue
      mkName()
      drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)
    }

    def name = _name // f"${_value}%1.5f"

    init()
  }
}