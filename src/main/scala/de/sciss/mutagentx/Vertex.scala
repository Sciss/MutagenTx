/*
 *  Vertex.scala
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

import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, InMemory, Obj, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}
import de.sciss.synth.{GE, UGenSpec}

object Vertex extends Obj.Type {
  final val typeID = 0x40001

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Vertex[S] = {
    val cookie = in.readByte()
    require (cookie == 3)
    val id  = tx.readID(in, access)
    val tpe = in.readByte()
    tpe match {
      case 0 =>
        val f = tx.readVar[Float](id, in)
        new Constant[S](id, f)
      case 1 =>
        // val name  = in.readUTF()
        UGen.readIdentified(id, in, access)
    }
  }

  implicit def Ser[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Vertex[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Vertex[S]] {
    def tpe = Vertex
  }

  object UGen {
    def apply[S <: Sys[S]](info: UGenSpec)(implicit tx: S#Tx): UGen[S] = {
      val index = UGens.seq.indexOf(info)
      new Impl[S](tx.newID(), index, info)
    }

    def unapply[S <: Sys[S]](v: UGen[S]): Option[UGenSpec] = Some(v.info)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): UGen[S] = {
      val tpe   = in.readInt()
      if (tpe != Vertex.typeID) sys.error(s"Type mismatch, found $tpe, expected ${Vertex.typeID}")
      val cookie  = in.readByte()
      if (cookie != 3) sys.error(s"Unexpected cookie, found $cookie, expected 3")
      val id    = tx.readID(in, access)
      val tpe1  = in.readByte()
      if (tpe1 != 1) sys.error(s"Expected Vertex.UGen cookie 1 but found $tpe1")
      readIdentified(id, in, access)
    }

    private[Vertex] def readIdentified[S <: Sys[S]](id: S#ID, in: DataInput, access: S#Acc)
                                                   (implicit tx: S#Tx): UGen[S] = {
      val index = in.readShort()
      val spec  = UGens.seq(index)
      new UGen.Impl[S](id, index, spec)
    }

    private[Vertex] final class Impl[S <: Sys[S]](val id: S#ID, index: Int, val info: UGenSpec)
      extends UGen[S] with evt.impl.ConstObjImpl[S, Any] {

      private def isBinaryOp: Boolean = info.name.startsWith("Bin_")
      private def isUnaryOp : Boolean = info.name.startsWith("Un_")

      def isUGen = true

      def tpe = Vertex

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Impl[Out](txOut.newID(), index = index, info = info)

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)
        // out.writeUTF(info.name)
        out.writeShort(index)
      }

      def instantiate(ins: Vec[(AnyRef, Class[_])]): GE =
        if      (isBinaryOp) mkBinaryOpUGen(ins)
        else if (isUnaryOp ) mkUnaryOpUGen (ins)
        else mkRegularUGen(ins)

      //      def asCompileString(ins: Vec[String]): String =
      //        if (isBinOp) mkBinOpString(ins) else mkRegularString(ins)

      def boxName =
        if (isBinaryOp) {
          val id  = info.name.substring(4).toInt
          val op  = BinaryOpUGen.Op(id)
          val n   = op.name
          s"${n.substring(0, 1).toLowerCase}${n.substring(1)}"
        } else if (isUnaryOp) {
          val id  = info.name.substring(3).toInt
          val op  = UnaryOpUGen.Op(id)
          val n   = op.name
          s"${n.substring(0, 1).toLowerCase}${n.substring(1)}"
        } else {
          info.name
        }

      private def mkBinaryOpUGen(ins: Vec[(AnyRef, Class[_])]): GE = {
        val id = info.name.substring(4).toInt
        val op = BinaryOpUGen.Op(id)
        op.make(ins(0)._1.asInstanceOf[GE], ins(1)._1.asInstanceOf[GE])
      }

      private def mkUnaryOpUGen(ins: Vec[(AnyRef, Class[_])]): GE = {
        val id = info.name.substring(3).toInt
        val op = UnaryOpUGen.Op(id)
        op.make(ins(0)._1.asInstanceOf[GE])
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
  trait UGen[S <: Sys[S]] extends Vertex[S] with stm.Mutable[S#ID, S#Tx] {
    def info: UGenSpec

    def boxName: String

    override def toString = s"${info.name}@${hashCode().toHexString}"

    def instantiate(ins: Vec[(AnyRef, Class[_])]): GE

    // def asCompileString(ins: Vec[String]): String

    //    def copy1()(implicit tx: S#Tx): UGen[S] = UGen(info)
    //
    //    def copyT[T <: Sys[T]]()(implicit stx: S#Tx, ttx: T#Tx): Vertex[T] = UGen[T](info)
  }
  //  class UGen(val info: UGenSpec) extends Vertex {
  //    override def toString = s"${info.name}@${hashCode().toHexString}"
  //  }
  object Constant {
    def apply[S <: Sys[S]](f: Float)(implicit tx: S#Tx): Constant[S] = {
      if (f.isNaN) throw new IllegalArgumentException
      val id = tx.newID()
      new Constant[S](id, tx.newVar(id, f))
    }
    def unapply[S <: Sys[S]](v: Constant[S])(implicit tx: S#Tx): Option[Float] = Some(v.f)
  }
  class Constant[S <: Sys[S]] private[Vertex] (val id: S#ID, fv: S#Var[Float])
    extends Vertex[S]
    with evt.impl.ConstObjImpl[S, Any] {

    def tpe = Vertex

    override def toString() = s"Constant$id"

    def f(implicit tx: S#Tx): Float = fv()
    def f_=(value: Float)(implicit tx: S#Tx): Unit = {
      val f1 = if (value.isNaN) {
        Console.err.println("Trying to set a constant to NaN")
        new Exception().printStackTrace()
        0f
      } else value

      fv() = f1
    }

    def isUGen = false

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val newID   = txOut.newID()
      val newVar  = txOut.newVar(newID, fv())
      new Constant[Out](newID, newVar)
    }

    //    def copy1()(implicit tx: S#Tx): Constant[S] = Constant(f())
    //
    //    def copyT[T <: Sys[T]]()(implicit stx: S#Tx, ttx: T#Tx): Constant[T] = Constant[T](f())

    // def boxName = f.toString
    protected def disposeData()(implicit tx: S#Tx): Unit = fv.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)
      fv.write(out)
    }
  }
}
sealed trait Vertex[S <: Sys[S]] extends Obj[S] {
  //  /** Creates an structurally identical copy, but wrapped in a new vertex (object identity).
  //    * Theoretically, a better approach would be fork and merge, but it doesn't fit well
  //    * into the current implementation of mutation.
  //    */
  //  def copy1()(implicit tx: S#Tx): Vertex[S]
  //
  //  def copyT[T <: Sys[T]]()(implicit stx: S#Tx, ttx: T#Tx): Vertex[T]

  def isUGen: Boolean
  def isConstant: Boolean = !isUGen

  // def boxName: String
}
