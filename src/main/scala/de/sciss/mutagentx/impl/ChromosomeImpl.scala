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

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm.{Copy, Elem, Sys}
import de.sciss.lucre.{event => evt, expr}
import de.sciss.synth
import de.sciss.synth.ugen.{BinaryOpUGen, ConfigOut, Constant, Mix, Nyquist, RandSeed, UnaryOpUGen}
import de.sciss.synth.{GE, SynthGraph, UGenSpec, UndefinedRate}

import scala.annotation.tailrec
import scala.util.control.NonFatal

object ChromosomeImpl {
  def elemName(in: GE): String =
    in match {
      case bin: BinaryOpUGen =>
        s"Bin_${bin.selector.id}"
      case un: UnaryOpUGen =>
        s"Un_${un.selector.id}"
      case _ => in.productPrefix
    }

  /** Reconstructs a chromosome from a given graph. Note that this
    * does not know which default arguments were present and which not,
    * therefore it simply omits producing constants for the default
    * values.
    */
  def mkChromosome[S <: Sys[S]](g: SynthGraph)(implicit tx: S#Tx): Chromosome[S] = {
    val c = Chromosome.empty[S]
    val m = new java.util.IdentityHashMap[Product, Vertex[S]]

    def addOne(p: Product): Option[Vertex[S]] = Option(m.get(p)).orElse(p match {
      // remove 'preamble' and output
      case _: ConfigOut | _: Mix | _: Mix.Mono | _: RandSeed => None

      case Constant(f) =>
        val f1 = if (f.isNaN) {
          Console.err.println("Warning: replacing NaN by zero in mkChromosome")
          0f
        } else f

        val v = Vertex.Constant(f1)
        m.put(p, v)
        c.addVertex(v)
        Some(v)
      case ge: GE =>
        val name = elemName(ge)
        val info = UGens.mapAll(name)
        val v    = Vertex.UGen(info)
        m.put(p, v)
        c.addVertex(v)
        v.info.args.foreach { arg =>
          arg.tpe match {
            case _: UGenSpec.ArgumentType.GE =>
              lazy val value: Any = {
                val m = ge.getClass.getMethod(arg.name)
                m.invoke(ge)
              }

              val isDefault = arg.defaults.get(ge.rate).exists { av =>
                val avc = av match {
                  case UGenSpec.ArgumentValue.Int(i)        => Constant(i)
                  case UGenSpec.ArgumentValue.Float(f)      => Constant(f)
                  case UGenSpec.ArgumentValue.Inf           => Constant(de.sciss.synth.inf)
                  case UGenSpec.ArgumentValue.Nyquist       => Nyquist()
                  case UGenSpec.ArgumentValue.Boolean(b)    => Constant(if (b) 1f else 0f)
                  case UGenSpec.ArgumentValue.String(s)     => s
                  case UGenSpec.ArgumentValue.DoneAction(d) => synth.DoneAction.toGE(d)
                }
                avc == value
              }
              if (!isDefault) value match {
                case p1: Product =>
                  val childVertex = addOne(p1).getOrElse(sys.error(s"Cannot create vertex for $p1"))
                  c.addEdge(Edge.make(v, childVertex, arg.name))

                case _ => sys.error(s"Unsupported product element $value in $ge")
              }

            case _ =>  // ignore Int
          }
        }
        Some(v)
    })

    g.sources.foreach(addOne)
    c
  }

  def findIncompleteUGenInputs[S <: Sys[S]](t1: Chromosome[S], v: Vertex.UGen[S])(implicit tx: S#Tx): Vec[String] = {
    val spec      = v.info
    val edgeSet   = t1.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
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

  def getRoots[S <: Sys[S]](top: Chromosome[S])(implicit tx: S#Tx): Vec[Vertex.UGen[S]] = {
    val ugens = top.vertices.iterator.collect {
      case ugen: Vertex.UGen[S] => ugen
    }
    val edges = top.edges.iterator.toList
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq.reverse
  }

  def addVertex[S <: Sys[S]](config: Algorithm.Config, c: Chromosome[S])
                            (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Vertex[S] = {
    import Util.coin
    import config.constProb

    if (coin(constProb)) {
      val _v = mkConstant()
      c.addVertex(_v)
      _v

    } else {
      val _v  = mkUGen()
      c.addVertex(_v)
      completeUGenInputs(config, c, _v)
      _v
    }
  }

  def completeUGenInputs[S <: Sys[S]](config: Algorithm.Config, c: Chromosome[S], v: Vertex.UGen[S])
                                    (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
    import Util.{choose, coin}
    import config.nonDefaultProb

    val spec    = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeSet = c.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
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
          try {
            c.addEdge(e) // .get
          } catch {
            case NonFatal(ex) =>
              println(c.debugString)
              throw ex
          }
        }

        loopVertex(tail)

      case _ =>
    }

    if (findDef.isEmpty) false else {
      loopVertex(findDef)
      true
    }
  }

  def mkUGen[S <: Sys[S]]()(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Vertex.UGen[S] = {
    import Util.choose
    val spec    = choose(UGens.seq)
    val v       = Vertex.UGen[S](spec)
    v
  }

  def mkConstant[S <: Sys[S]]()(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Vertex.Constant[S] = {
    val v = Vertex.Constant[S](mkConstantValue())
    v
  }

  def mkConstantValue[S <: Sys[S]]()(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Float = {
    import Util.{coin, exprand}
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    f.toFloat
  }
}
final class ChromosomeImpl[S <: Sys[S]](val id      : S#ID,
                                        val vertices: expr.List.Modifiable[S, Vertex[S]],
                                        val edges   : expr.List.Modifiable[S, Edge  [S]],
                                        val unconnected: S#Var[Int],
                                        val sourceEdgeMap: SkipList.Map[S, Int, Map[Vertex[S], Set[Edge[S]]]],
                                        val targetEdgeMap: SkipList.Map[S, Int, Map[Vertex[S], Set[Edge[S]]]])
  extends TopologyImpl[S, Vertex[S], Edge[S]]
    with Chromosome[S] with  evt.impl.ConstObjImpl[S, Any] { in =>

  override def toString = s"Chromosome$id"

  def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
    val idOut = txOut.newID()
    type VListAux[~ <: Sys[~]] = expr.List.Modifiable[~, Vertex[~]]
    type EListAux[~ <: Sys[~]] = expr.List.Modifiable[~, Edge  [~]]
    val vOut  = context[VListAux](vertices)
    val eOut  = context[EListAux](edges   )
    val uOut  = txOut.newVar(idOut, unconnected())
    val mSOut = SkipList.Map.empty[Out, Int, Map[Vertex[Out], Set[Edge[Out]]]]
    val mTOut = SkipList.Map.empty[Out, Int, Map[Vertex[Out], Set[Edge[Out]]]]
    val out   = new ChromosomeImpl[Out](idOut, vOut, eOut, uOut, mSOut, mTOut)
    context.defer(in, out) {
      def copyMap(mIn: EdgeMap, mOut: SkipList.Map[Out, Int, Map[Vertex[Out], Set[Edge[Out]]]]): Unit =
        mIn.iterator.foreach { case (_, miIn) =>
          miIn.foreach { case (vIn, eIn) =>
            val vOut    = context(vIn)
            val eOut    = eIn.map(context(_))
            val keyOut  = vOut.hashCode()
            val m0      = mOut.get(keyOut).getOrElse(Map.empty)
            val s0      = m0.getOrElse(vOut, Set.empty)
            val s1      = s0 ++ eOut
            val miOut   = m0 + (vOut -> s1)
            mOut.add(keyOut -> miOut)
          }
        }

      copyMap(in.sourceEdgeMap, out.sourceEdgeMap)
      copyMap(in.targetEdgeMap, out.targetEdgeMap)
    }
    out
  }
}
