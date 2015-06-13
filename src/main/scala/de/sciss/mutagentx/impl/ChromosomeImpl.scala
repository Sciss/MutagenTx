/*
 *  ChromosomeImpl.scala
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
package impl

import java.util

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, SampleRate}
import de.sciss.synth.{doNothing, GE, Lazy, Rate, SynthGraph, UGenSpec, UndefinedRate, ugen}

import scala.annotation.tailrec

object ChromosomeImpl {
  def mkSynthGraph(c: Chromosome, mono: Boolean, removeNaNs: Boolean, config: Boolean)
                  (implicit tx: S#Tx /*, random: TxnRandom[D#Tx] */): SynthGraph = {
    val top = c

    @tailrec def loop(rem: Vec[Vertex], real: Map[Vertex, GE]): Map[Vertex, GE] = rem match {
      case init :+ last =>
        val value: GE = last match {
          case Vertex.Constant(f) => ugen.Constant(f)
          case u @ Vertex.UGen(spec) =>
            val ins = spec.args.map { arg =>
              val res: (AnyRef, Class[_]) = arg.tpe match {
                case UGenSpec.ArgumentType.Int =>
                  val v = arg.defaults.get(UndefinedRate) match {
                    case Some(UGenSpec.ArgumentValue.Int(i)) => i
                    case _ => 1 // rrand(1, 2)
                  }
                  (v.asInstanceOf[AnyRef], classOf[Int])

                case UGenSpec.ArgumentType.GE(_, _) =>
                  val lastE   = top.edgeMap.get(last)
                  val inGEOpt = lastE.flatMap { set =>
                    set.flatMap { e =>
                      if (e.inlet == arg.name) real.get(e.targetVertex) else None
                    } .headOption
                  }
                  val inGE = inGEOpt.getOrElse {
                    val xOpt = arg.defaults.get(UndefinedRate)
                    val x    = xOpt.getOrElse {
                      val inc = findIncompleteUGenInputs(top, u)
                      println("INCOMPLETE:")
                      inc.foreach(println)
                      println(top.debugString)
                      sys.error(s"Vertex $spec has no input for inlet $arg")
                    }
                    x match {
                      case UGenSpec.ArgumentValue.Boolean(v)    => ugen.Constant(if (v) 1 else 0)
                      case UGenSpec.ArgumentValue.DoneAction(v) => ugen.Constant(v.id)
                      case UGenSpec.ArgumentValue.Float(v)      => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Inf           => ugen.Constant(Float.PositiveInfinity)
                      case UGenSpec.ArgumentValue.Int(v)        => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Nyquist       => SampleRate.ir / 2
                      case UGenSpec.ArgumentValue.String(v)     => ugen.Escape.stringToGE(v)
                    }
                  }
                  (inGE, classOf[GE])
              }
              res
            }

            u.instantiate(ins)
        }

        loop(init, real + (last -> value))

      case _ =>  real
    }

    SynthGraph {
      import de.sciss.synth.ugen._
      RandSeed.ir()
      val vertices = top.vertices.iterator.toIndexedSeq
      val map   = loop(vertices, Map.empty)
      val ugens = vertices.collect {
        case ugen: Vertex.UGen => ugen
      }
      if (ugens.nonEmpty) {
        val roots = getRoots(top)
        val sig0: GE = if (roots.isEmpty) map(ugens.head /* choose(ugens) */) else Mix(roots.map(map.apply))
        val sig1  = /* if (mono) */ Mix.mono(sig0) /* else sig0 */
        val sig2  = if (!removeNaNs) sig1 else {
            val isOk = CheckBadValues.ar(sig1, post = 0) sig_== 0
            Gate.ar(sig1, isOk)
          }
        val sig3  = if (config) sig2 else Limiter.ar(LeakDC.ar(sig2))
        val sig   = if (mono) sig3 else Pan2.ar(sig3) // SplayAz.ar(numChannels = 2, in = sig3)
        if (config)
          ConfigOut(sig)
        else
          Out.ar(0, sig)
      }
    }
  }

  def findIncompleteUGenInputs(t1: Chromosome, v: Vertex.UGen)(implicit tx: S#Tx): Vec[String] = {
    val spec      = v.info
    val edgeSet   = t1.edgeMap.get(v).getOrElse(Set.empty)
    val argsFree  = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val inc       = argsFree.filterNot(_.defaults.contains(UndefinedRate))
    inc.map(_.name)
  }

  def geArgs(spec: UGenSpec): Vec[UGenSpec.Argument] = {
    val res       = spec.args.filter { arg =>
      arg.tpe match {
        case UGenSpec.ArgumentType.Int => false
        case UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.DoneAction, _) => false
        case _ => true
      }
    }
    res
  }

  private def getRoots(top: Chromosome)(implicit tx: S#Tx): Vec[Vertex.UGen] = {
    val ugens = top.vertices.iterator.collect {
      case ugen: Vertex.UGen => ugen
    }
    val edges = top.edges.iterator.toList
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq
  }

  def addVertex(c: Chromosome)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Vertex = {
    import Algorithm.constProb
    import Util.coin

    if (coin(constProb)) {
      val _v = mkConstant()
      c.addVertex(_v)
      _v

    } else {
      val _v  = mkUGen()
      c.addVertex(_v)
      completeUGenInputs(c, _v)
      _v
    }
  }

  def completeUGenInputs(c: Chromosome, v: Vertex.UGen)(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Boolean = {
    import Algorithm.nonDefaultProb
    import Util.{choose, coin}

    val spec    = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeSet = c.edgeMap.get(v).getOrElse(Set.empty)
    val argsFree = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val (hasDef, hasNoDef)          = argsFree.partition(_.defaults.contains(UndefinedRate))
    val (useNotDef, _ /* useDef */) = hasDef.partition(_ => coin(nonDefaultProb))
    val findDef = hasNoDef ++ useNotDef

    @tailrec def loopVertex(rem: Vec[UGenSpec.Argument]): Unit = rem match {
      case head +: tail =>
        val options = c.vertices.iterator.filter { vi =>
          val e = Edge.make(v, vi, head.name)
          c.canAddEdge(e)
        }
        if (options.nonEmpty) {
          val vi  = choose(options.toIndexedSeq)
          val e   = Edge.make(v, vi, head.name)
          c.addEdge(e) // .get
        } else {
          val vi  = mkConstant()
          c.addVertex(vi)
          val e   = Edge.make(v, vi, head.name)
          c.addEdge(e) // .get
        }

        loopVertex(tail)

      case _ =>
    }

    if (findDef.isEmpty) false else {
      loopVertex(findDef)
      true
    }
  }

  def mkUGen()(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Vertex.UGen = {
    import Util.choose
    val spec    = choose(UGens.seq)
    val v       = Vertex.UGen(spec)
    v
  }

  def mkConstant()(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Vertex.Constant = {
    val v = Vertex.Constant(mkConstantValue())
    v
  }

  def mkConstantValue()(implicit tx: S#Tx, random: TxnRandom[D#Tx]): Float = {
    import Util.{coin, exprand}
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    f.toFloat
  }

  /** Tries to generate a source-code string from a given `SynthGraph`. If `wrap`
    * is `false`, the body of the graph is returned, if it is `true` (default),
    * the body is wrapped in a `"SynthGraph { ... }"` string.
    * If `imports` is `true`, the body will be preceded by the statements
    * `"import de.sciss.synth._; import ugen._"`
    */
  def graphToString(g: SynthGraph, imports: Boolean = false, wrap: Boolean = true): String = {
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
      val invoke  = if (line.elemName == "BinaryOpUGen") {
        line.args.head.value match {
          case op: BinaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a0, b) = args
            // XXX TODO --- stupid workaround for ScalaCollider #52
            val a = if ((opS == "min" || opS == "max") && line.args(1).value.isInstanceOf[Constant])
                s"Constant($a0)" else a0
            s"$a $opS $b"
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