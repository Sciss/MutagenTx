/*
 *  Vertex.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx

import de.sciss.lucre.stm.{Mutable, Identifiable}
import de.sciss.serial.{Writable, DataInput, Serializer, DataOutput}
import de.sciss.synth.GE
import de.sciss.synth.UGenSpec
import de.sciss.synth.ugen.BinaryOpUGen

import scala.collection.immutable.{IndexedSeq => Vec}

//object Vertex {
//  // strangely, a constant is mutable, while a ugen is constant
//
//  trait Constant extends Vertex with Identifiable[S#Tx] {
//    def value: S#Var[Double]
//  }
//
//  trait UGen extends Vertex {
//    def name: String
//  }
//}
//sealed trait Vertex {
//
//}

object Vertex {
  implicit object Ser extends Serializer[S#Tx, S#Acc, Vertex] {
    def write(v: Vertex, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Vertex = {
      val id  = tx.readID(in, access)
      val tpe = in.readByte()
      tpe match {
        case 0 =>
          val f = tx.readVar[Float](id, in)
          new Constant(id, f)
        case 1 =>
          val name  = in.readUTF()
          val spec  = UGens.map.getOrElse(name, sys.error(s"Spec '$name' not registered"))
          new UGen.Impl(id, spec)
      }
    }
  }

  object UGen {
    def apply(info: UGenSpec)(implicit tx: S#Tx): UGen = new Impl(tx.newID(), info)
    def unapply(v: UGen): Option[UGenSpec] = Some(v.info)

    private[Vertex] final class Impl(val id: S#ID, val info: UGenSpec) extends UGen with Mutable.Impl[S] {
      private def isBinOp: Boolean = info.name.startsWith("Bin_")

      // def copy(): UGen = new Impl(info)

      protected def disposeData()(implicit tx: S#Tx) = ()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)
        out.writeUTF(info.name)
      }

      def instantiate(ins: Vec[(AnyRef, Class[_])]): GE =
        if (isBinOp) mkBinOpUGen(ins) else mkRegularUGen(ins)

      //      def asCompileString(ins: Vec[String]): String =
      //        if (isBinOp) mkBinOpString(ins) else mkRegularString(ins)

      def boxName =
        if (isBinOp) {
          val id = info.name.substring(4).toInt
          val op = BinaryOpUGen.Op(id)
          val n   = op.name
          s"${n.substring(0, 1).toLowerCase}${n.substring(1)}"
        } else {
          info.name
        }

      //      private def mkBinOpString(ins: Vec[String]): String = {
      //        val nu = boxName
      //        s"(${ins(0)} $nu ${ins(1)})"
      //      }

      //      private def mkRegularString(ins: Vec[String]): String = {
      //        val rates = info.rates
      //        val consName0 = rates.method match {
      //          case UGenSpec.RateMethod.Alias (name) => name
      //          case UGenSpec.RateMethod.Custom(name) => name
      //          case UGenSpec.RateMethod.Default =>
      //            val rate = rates.set.max
      //            rate.methodName
      //        }
      //        val consName  = if (consName0 == "apply") "" else s".$consName0"
      //        val nameCons  = s"${info.name}$consName"
      //        if (ins.isEmpty && consName.nonEmpty)   // e.g. SampleRate.ir
      //          nameCons
      //        else
      //          ins.mkString(s"$nameCons(", ", ", ")")
      //      }

      private def mkBinOpUGen(ins: Vec[(AnyRef, Class[_])]): GE = {
        val id = info.name.substring(4).toInt
        val op = BinaryOpUGen.Op(id)
        op.make(ins(0)._1.asInstanceOf[GE], ins(1)._1.asInstanceOf[GE])
      }

      private def mkRegularUGen(ins: Vec[(AnyRef, Class[_])]): GE = {
        val consName = info.rates.method match {
          case UGenSpec.RateMethod.Alias (name) => name
          case UGenSpec.RateMethod.Custom(name) => name
          case UGenSpec.RateMethod.Default =>
            val rate = info.rates.set.max
            rate.methodName
        }

        val (consValues, consTypes) = ins.unzip

        // yes I know, we could use Scala reflection
        val companionName   = s"de.sciss.synth.ugen.${info.name}$$"
        val companionClass  = Class.forName(companionName)
        val companionMod    = companionClass.getField("MODULE$").get(null)
        val cons            = companionClass.getMethod(consName, consTypes: _*)
        val ge              = cons.invoke(companionMod, consValues: _*).asInstanceOf[GE]
        ge
      }
    }
  }
  trait UGen extends Vertex {
    def info: UGenSpec

    override def toString = s"${info.name}@${hashCode().toHexString}"

    def instantiate(ins: Vec[(AnyRef, Class[_])]): GE

    // def asCompileString(ins: Vec[String]): String
  }
  //  class UGen(val info: UGenSpec) extends Vertex {
  //    override def toString = s"${info.name}@${hashCode().toHexString}"
  //  }
  object Constant {
    def apply(f: Float)(implicit tx: S#Tx): Constant = {
      val id = tx.newID()
      new Constant(id, tx.newVar(id, f))
    }
    def unapply(v: Constant)(implicit tx: S#Tx): Option[Float] = Some(v.f())
  }
  class Constant(val id: S#ID, val f: S#Var[Float]) extends Vertex with Mutable.Impl[S] {
    override def toString() = s"Constant$id"
    // def copy(): Constant = new Constant(f)

    // def boxName = f.toString
    protected def disposeData()(implicit tx: S#Tx): Unit = f.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)
      f.write(out)
    }
  }
}
sealed trait Vertex extends Identifiable[S#ID] with Writable {
  //  /** Creates an structurally identical copy, but wrapped in a new vertex (object identity). */
  //  def copy(): Vertex

  // def boxName: String
}
