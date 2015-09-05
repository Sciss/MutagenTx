package de.sciss.mutagentx

import java.util

import de.sciss.synth.ugen.{BinaryOpUGen, Constant, UnaryOpUGen}
import de.sciss.synth.{GE, Lazy, Rate, SynthGraph, UGenSpec, doNothing}

object MkGraphSource {
  /** Tries to generate a source-code string from a given `SynthGraph`. If `wrap`
    * is `false`, the body of the graph is returned, if it is `true` (default),
    * the body is wrapped in a `"SynthGraph { ... }"` string.
    * If `imports` is `true`, the body will be preceded by the statements
    * `"import de.sciss.synth._; import ugen._"`
    */
  def apply(g: SynthGraph, imports: Boolean = false, wrap: Boolean = true): String = {
    case class ArgAssign(name: Option[String], shape: UGenSpec.SignalShape, value: Any)

    class GraphLine(val elemName: String, val constructor: String, val args: Vec[ArgAssign]) {
      var uses    = Set.empty[String]
      var valName = Option.empty[String]
    }

    val ugenMap = UGenSpec.standardUGens

    var lines   = Vec.empty[GraphLine]
    // var lineMap = Map.empty[Lazy, GraphLine]
    val lineMap = new util.IdentityHashMap[Lazy, GraphLine]

    g.sources.zipWithIndex.foreach { case (elem, elemIdx) =>
      val elemName  = elem.productPrefix
      val argVals   = elem.productIterator.toIndexedSeq
      // val valName   = s"val e$elemIdx"

      val line = ugenMap.get(elemName).fold[GraphLine] {
        //        val companion = Class.forName(s"$elemName$$")
        //        val method = companion.getMethods.find { m =>
        //          m.getName == "apply" && m.getParameterTypes.length == argVals.size
        //        } .getOrElse(sys.error(s"Cannot decode element $elemName"))
        //        val ins = method.get

        //        elem match {
        //          case bin  : BinaryOpUGen  =>
        //            bin.selector.name
        //
        //          case unary: UnaryOpUGen   =>
        //          case other =>
        //        }

        val ins = argVals.map(new ArgAssign(None, UGenSpec.SignalShape.Generic, _))

        new GraphLine(elemName = elemName, constructor = "apply", args = ins)
      } { spec =>
        val (rate: Rate, rateMethod: UGenSpec.RateMethod, argVals1: Vec[Any]) = spec.rates match {
          case UGenSpec.Rates.Implied(r, m) => (r, m, argVals)
          case UGenSpec.Rates.Set(_) =>
            argVals.head match {
              case r: Rate => (r, UGenSpec.RateMethod.Default, argVals.tail)
            }
        }
        val rateMethodName = rateMethod match {
          case UGenSpec.RateMethod.Alias (name) => name
          case UGenSpec.RateMethod.Custom(name) => name
          case UGenSpec.RateMethod.Default      => rate.methodName
        }
        val ins = (spec.args zip argVals1).map { case (arg, argVal) =>
          //          val res: String = arg.tpe match {
          //            case UGenSpec.ArgumentType.Int =>
          //              argVal.toString
          //
          //            case UGenSpec.ArgumentType.GE(shape, _) =>
          ////              shape match {
          ////                case UGenSpec.SignalShape.
          ////              }
          //              argVal.toString
          //          }
          //          val res = argVal match {
          //            case Constant(c) => c.toString
          //            case other =>
          //              val elemIdx1 = g.sources.indexOf(argVal)
          //              s"e$elemIdx1"
          //          }
          //          s"${arg.name} = $res"
          val shape = arg.tpe match {
            case UGenSpec.ArgumentType.GE(sh, _) => sh
            case _ => UGenSpec.SignalShape.Generic
          }
          new ArgAssign(Some(arg.name), shape, argVal)
        }
        // b.append(ins.mkString(s"$valName = $elemName$methodCall(", ", ", ")\n"))
        new GraphLine(elemName = elemName, constructor = rateMethodName, args = ins)
      }

      lines  :+= line
      // lineMap += (elem -> line)
      lineMap.put(elem, line)

      line.args.foreach {
        case ArgAssign(argNameOpt, _, argVal: Lazy) =>
          // val ref     = lineMap(argVal)
          val ref     = lineMap.get(argVal)
          val argName = argNameOpt.getOrElse("unnamed")
          ref.uses   += argName

        case ArgAssign(_, _, argVal: Product) if argVal.productPrefix == "GESeq" => // XXX TODO -- quite hackish
          val elems = argVal.productIterator.next().asInstanceOf[Vec[GE]]
          elems.foreach {
            case elem: Lazy =>
              // val ref     = lineMap(elem)
              val ref     = lineMap.get(elem)
              ref.uses   += "unnamed"
            case _ =>
          }

        case _ =>
      }
    }

    def uncapitalize(in: String): String = if (in.isEmpty) in else
      in.updated(0, Character.toLowerCase(in.charAt(0)))

    // assign preliminary val-names
    lines.foreach { line =>
      val uses = line.uses
      if (uses.nonEmpty) (uses - "unnamed").toList match {
        case single :: Nil if single != "unnamed" => line.valName = Some(single)
        case multiple =>
          val nameUp0 = if (line.elemName == "BinaryOpUGen") {
            val x = line.args.head.value.getClass.getName
            /* x.asInstanceOf[Product].productPrefix */
            x.substring(0, x.length - 1)
          } else line.elemName

          val di      = nameUp0.lastIndexOf('$')
          val nameUp  = nameUp0.substring(di + 1)
          val nameLo  = uncapitalize(nameUp)
          line.valName = Some(nameLo)
      }
    }
    // make sure val-names are unique
    lines.zipWithIndex.foreach { case (line, li) =>
      line.valName.foreach { name0 =>
        val same = lines.filter(_.valName == line.valName)
        // cf. https://issues.scala-lang.org/browse/SI-9353
        val si9353 = lines.iterator.zipWithIndex.exists { case (line1, lj) =>
          lj < li && line1.args.exists(_.name == line.valName)
        }
        if (same.size > 1 || si9353) {
          same.zipWithIndex.foreach { case (line1, i) =>
            line1.valName = Some(s"${name0}_$i")
          }
        }
      }
    }
    // calc indentation
    val maxValNameSz0 = (0 /: lines)((res, line) => line.valName.fold(res)(n => math.max(n.length, res)))
    val maxValNameSz  = maxValNameSz0 | 1 // odd

    // turn to source
    val linesS0 = lines.map { line =>
      val numArgs = line.args.size
      val args    = line.args.zipWithIndex.map { case (arg, ai) =>
        def mkString(x: Any): String = x match {
          case Constant(c) =>
            import UGenSpec.SignalShape._
            arg.shape match {
              case Int | Trigger | Gate | Switch if c == c.toInt => c.toInt.toString
              case DoneAction =>
                val id = c.toInt
                if (id == doNothing.id) doNothing.toString else id.toString // XXX TODO -- DoneAction.apply missing
              case _ => c.toString
            }

          case l: Lazy =>
            // val line1 = lineMap(l)
            val line1 = lineMap.get(l)
            line1.valName.get

          case sq: Product if sq.productPrefix == "GESeq" =>
            val peer = sq.productIterator.next().asInstanceOf[Vec[GE]]
            peer.map(mkString).mkString("Seq[GE](", ", ", ")")

          case other =>
            other.toString
        }
        val valString = mkString(arg.value)
        if (numArgs == 1) valString else arg.name.fold(valString) { argName =>
          if (ai == 0 && argName == "in") valString else s"$argName = $valString"
        }
      }
      val invoke = if (line.elemName == "BinaryOpUGen") {
        line.args.head.value match {
          case op: BinaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a0, b) = args
            // XXX TODO --- stupid workaround for ScalaCollider #52
            val a = if ((opS == "min" || opS == "max") && line.args(1).value.isInstanceOf[Constant])
              s"Constant(${a0}f)"
            else a0
            s"$a $opS $b"
        }
      } else if (line.elemName == "UnaryOpUGen") {
        line.args.head.value match {
          case UnaryOpUGen.Neg =>
            val Seq(_, a) = args
            s"-$a"
          case UnaryOpUGen.Not =>
            val Seq(_, a) = args
            s"UnaryOpUGen($a, UnaryOpUGen.Not)"
          case op: UnaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a) = args
            s"$a.$opS"
        }
      } else {
        val cons      = if (line.constructor == "apply") "" else s".${line.constructor}"
        val elemName  = line.elemName.replace('$', '.')
        val select    = s"$elemName$cons"
        if (args.isEmpty && cons.nonEmpty) select else args.mkString(s"$select(", ", ", ")")
      }
      line.valName.fold(invoke) { valName =>
        val pad = " " * (maxValNameSz - valName.length)
        s"val $valName$pad = $invoke"
      }
    }

    val linesS = if (imports) "import de.sciss.synth._; import ugen._" +: "" +: linesS0 else linesS0

    if (wrap)
      linesS.mkString("SynthGraph {\n  ", "\n  ", "\n}")
    else
      linesS.mkString("\n")
  }
}
