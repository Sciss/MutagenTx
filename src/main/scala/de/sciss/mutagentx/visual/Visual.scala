/*
 *  Visual.scala
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
package visual

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.{Color, Font, LayoutManager, RenderingHints}
import java.io.FileInputStream
import javax.imageio.ImageIO
import javax.swing.JPanel

import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx, requireEDT}
import de.sciss.mutagentx.visual.impl.{BoxRenderer, MyEdgeRenderer, MySpringForce}
import de.sciss.processor.Processor
import prefuse.action.assignment.ColorAction
import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.action.{ActionList, RepaintAction}
import prefuse.activity.Activity
import prefuse.controls.{DragControl, PanControl, WheelZoomControl, ZoomControl}
import prefuse.data.{Graph => PGraph}
import prefuse.render.DefaultRendererFactory
import prefuse.util.ColorLib
import prefuse.util.force.{DragForce, ForceSimulator, NBodyForce}
import prefuse.visual.expression.InGroupPredicate
import prefuse.visual.{VisualGraph, VisualItem}
import prefuse.{Constants, Display, Visualization}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.concurrent.stm.{Ref, TMap, TxnLocal}
import scala.concurrent.{Await, ExecutionContext, Promise, blocking}
import scala.swing.{Component, Dimension, Graphics2D, Rectangle, Swing}
import scala.util.Try
import scala.util.control.NonFatal

object Visual {
  val DEBUG = false

  // val VIDEO_WIDTH     = 720
  val VIDEO_HEIGHT    = 1920 / 2 // 576
  val VIDEO_WIDTH_SQR = 1080 / 2 // 1024 // 1024 : 576 = 16 : 9

  type S = ConfluentReactive

  def apply(a: Algorithm.Confluent)(implicit tx: S#Tx): Visual[S] = {
    val map = TMap.empty[S#ID, VisualVertex[S]]
    implicit val dtx = tx.durable
    new Impl(map, a, a.global.cursor.position).init()
  }

  private lazy val _initFont: Font = {
    //    val is  = Wolkenpumpe.getClass.getResourceAsStream("BellySansCondensed.ttf")
    //    val res = Font.createFont(Font.TRUETYPE_FONT, is)
    //    is.close()
//    val res = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
//    res
    //      // "SF Movie Poster Condensed"
    //      new Font( "BellySansCondensed", Font.PLAIN, 12 )
    val is  = new FileInputStream("dosis/Dosis-Medium.ttf")
    val res = Font.createFont(Font.TRUETYPE_FONT, is)
    is.close()
    res
  }

  private var _condensedFont: Font = _

  /** A condensed font for GUI usage. This is in 12 pt size,
    * so consumers must rescale.
    */
  def condensedFont: Font = {
    if (_condensedFont == null) _condensedFont = _initFont
    _condensedFont
  }
  def condensedFont_=(value: Font): Unit =
    _condensedFont = value

  final val GROUP_GRAPH   = "graph"
  final val COL_MUTA      = "muta"
  private final val GROUP_NODES   = "graph.nodes"
  private final val GROUP_EDGES   = "graph.edges"
  private final val ACTION_LAYOUT = "layout"
  private final val ACTION_COLOR  = "color"
  private final val LAYOUT_TIME   = 50

  private final class Impl(map: TMap[S#ID, VisualVertex[S]], val algorithm: Algorithm.Confluent, cursorPos0: S#Acc)
    extends Visual[S] with ComponentHolder[Component] {

    private[this] var _vis: Visualization       = _
    private[this] var _dsp: Display             = _
    private[this] var _g  : PGraph              = _
    private[this] var _vg : VisualGraph         = _
    private[this] var _lay: ForceDirectedLayout = _
    private[this] var actionColor: ActionList   = _
    private[this] var _runAnim = false

    private val cursorPos = Ref(cursorPos0)

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      // val c = algorithm.genome.chromosomes().head
      // insertChromosome(c)
      this
    }

    def initChromosome(idx: Int): Unit =
      algorithm.global.cursor.step { implicit tx =>
        val c = algorithm.genome.chromosomes().apply(idx)
        insertChromosome(c)
      }

    def forceSimulator: ForceSimulator = _lay.getForceSimulator

    private def insertChromosome(c: Chromosome[S])(implicit tx: S#Tx): Unit = {
      c.vertices.iterator.foreach { v =>
        if (v.isUGen || c.edges.iterator.filter(_.targetVertex == v).nonEmpty) {
          checkOrInsertVertex(v)
        }
      }
      c.edges.iterator.foreach { e =>
        checkOrInsertLink(e)
      }
    }

    def display       : Display       = _dsp
    def visualization : Visualization = _vis
    def graph         : PGraph        = _g
    def visualGraph   : VisualGraph   = _vg



    private[this] val guiCode = TxnLocal(init = Vector.empty[() => Unit], afterCommit = handleGUI)

    private[this] def handleGUI(seq: Vec[() => Unit]): Unit = {
      def exec(): Unit = _vis.synchronized {
        stopAnimation()
        seq.foreach { fun =>
          try {
            fun()
          } catch {
            case NonFatal(e) => e.printStackTrace()
          }
        }
        startAnimation()
      }

      defer(exec())
    }

    def deferVisTx(thunk: => Unit)(implicit tx: TxnLike): Unit =
      guiCode.transform(_ :+ (() => thunk))(tx.peer)

    @inline private def stopAnimation(): Unit = {
      _vis.cancel(ACTION_COLOR )
      _vis.cancel(ACTION_LAYOUT)
    }

    @inline private def startAnimation(): Unit =
      _vis.run(ACTION_COLOR)

    private def checkOrInsertVertex(v: Vertex[S])(implicit tx: S#Tx): Unit =
      map.get(v.id)(tx.peer).fold {
        insertVertex(v)
      } { vv =>
        (v, vv) match {
          case (vc: Vertex.Constant[S], vvc: VisualConstant[S]) =>
            val value = vc.f()
            if (vvc.value != value) {
              println(f"VALUE ${vvc.value}%1.3f >>> $value%1.3f")
              vvc.value = value   // XXX animate -- how? THREADING
            }
          case _ =>
        }
        vv.touch()
      }

    private def insertVertex(v: Vertex[S])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val vv = VisualVertex[S](this, v)
      val res = map.put(v.id, vv)
      require(res.isEmpty)
      deferVisTx(insertVertexGUI(vv, locO = None))
    }

    private def initNodeGUI(obj: VisualVertex[S], vn: VisualVertex[S], locO: Option[Point2D]): VisualItem = {
      val pNode = vn.pNode
      val _vi   = _vis.getVisualItem(GROUP_GRAPH, pNode)
      val same  = vn == obj
      locO.fold {
        if (!same) {
          val _vi1 = _vis.getVisualItem(GROUP_GRAPH, obj.pNode)
          _vi.setEndX(_vi1.getX)
          _vi.setEndY(_vi1.getY)
        }
      } { loc =>
        _vi.setEndX(loc.getX)
        _vi.setEndY(loc.getY)
      }
      _vi
    }

    private def insertVertexGUI(v: VisualVertex[S], locO: Option[Point2D]): Unit = {
      initNodeGUI(v, v, locO)
    }

    private def checkOrInsertLink(e: Edge[S])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val tup = for {
        sourceV <- map.get(e.sourceVertex.id)
        targetV <- map.get(e.targetVertex.id)
      } yield (sourceV, targetV)

      if (tup.isEmpty) {
        println(s"WARNING: link misses vertices: $e")
      }

      tup.foreach { case (sourceV, targetV) =>
        val edge = VisualEdge[S](sourceV, targetV, init = false)
        sourceV.edgesOut.get(edge.key).fold[Unit](edge.init())(_.touch())
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()

    private def shrink(in: S#Acc): S#Acc = {
      val prevIdx = in.take(in.size - 3)
      if (prevIdx.isEmpty) prevIdx else  prevIdx :+ prevIdx.term
    }

    def previousIteration(): Unit = {
      val pos = cursorPos.single.transformAndGet(c => shrink(c) /* c.take(c.size - 2) */)
      // if (pos.isEmpty) return
      if (pos.size <= 2) return

      algorithm.global.forkCursor.stepFrom(pos) { implicit tx =>
        implicit val itx = tx.peer
        val mapOld = map.snapshot
        map.clear()
        var toRemove = Set.empty[VisualData[S]]
        mapOld.foreach { case (idOld, v) =>
          val pathOld = idOld.path
          val pathNew = shrink(pathOld)
          if (pathNew.isEmpty) {
            toRemove += v
          } else {
            val idNew = idOld.copy(pathNew)
            if (map.contains(idNew)) {
              toRemove += v
            } else {
              map.put(idNew, v)
            }
          }
        }

        // for (i <- 1 to 1) {
        val cs      = algorithm.genome.chromosomes()
        val csSz    = cs.size
        var lastProg= 0
        var csi     = 0
        val ancestors = cs.filter { c =>
          val vs = c.vertices
          val res = vs.iterator.filter { v =>
            val vid = v.id
            map.contains(vid)
          } .nonEmpty
          csi += 1
          val prog = csi * 100 / csSz
          while (lastProg < prog) {
            print('#')
            lastProg += 1
          }
          res
        }
        println(s"\nNum-ancestors = ${ancestors.size}")
        ancestors.foreach { c =>
          insertChromosome(c)
        }
        // }

        map.foreach { case (_, v) =>
          if (!v.isActive) toRemove += v

          def checkEdges(es: TMap[_, VisualEdge[S]]): Unit = es.foreach {
            case (_, e) => if (!e.isActive) toRemove += e
          }

          checkEdges(v.edgesIn )
          checkEdges(v.edgesOut)
        }

        if (DEBUG) {
          val numVertices = toRemove.count(_.isInstanceOf[VisualVertex[S]])
          val numEdges    = toRemove.count(_.isInstanceOf[VisualEdge  [S]])
          println(s"map.size = ${mapOld.size} -> ${map.size}; toRemove.size = $numVertices vertices / $numEdges edges")
        }

        toRemove.foreach(_.dispose())
      }
    }

    private def mkActionColor(): Unit = {
      // colors
      val actionNodeStrokeColor  = new ColorAction(GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      actionNodeStrokeColor.add(VisualItem.HIGHLIGHT, ColorLib.rgb(255, 255, 0))
      // actionNodeStroke.add(VisualItem.HIGHLIGHT, ColorLib.rgb(220, 220, 0))
      val actionNodeFillColor    = new ColorAction(GROUP_NODES, VisualItem.FILLCOLOR  , ColorLib.rgb(0, 0, 0))
      actionNodeFillColor.add(VisualItem.HIGHLIGHT, ColorLib.rgb(63, 63, 0))
      val actionTextColor   = new ColorAction(GROUP_NODES, VisualItem.TEXTCOLOR  , ColorLib.rgb(255, 255, 255))

      val actionEdgeStrokeColor = new ColorAction(GROUP_EDGES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      val actionEdgeFillColor   = new ColorAction(GROUP_EDGES, VisualItem.FILLCOLOR  , ColorLib.rgb(255, 255, 255))
      //      val actionAggrFill    = new ColorAction(AGGR_PROC  , VisualItem.FILLCOLOR  , ColorLib.rgb(80, 80, 80))
      //      val actionAggrStroke  = new ColorAction(AGGR_PROC  , VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))

      actionColor = new ActionList(_vis)
      actionColor.add(actionTextColor)
      actionColor.add(actionNodeStrokeColor)
      actionColor.add(actionNodeFillColor)
      actionColor.add(actionEdgeStrokeColor)
      actionColor.add(actionEdgeFillColor  )
      //      actionColor.add(actionAggrFill)
      //      actionColor.add(actionAggrStroke)
      // actionColor.add(_lay)
      _vis.putAction(ACTION_COLOR, actionColor)
    }

    private def guiInit(): Unit = {
      _vis = new Visualization
      _dsp = new Display(_vis) {
        override def setRenderingHints(g: Graphics2D): Unit = {
          super.setRenderingHints(g)
          g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
            if (m_highQuality) RenderingHints.VALUE_STROKE_PURE else RenderingHints.VALUE_STROKE_NORMALIZE)
        }
      }

      _g     = new PGraph(true)
      _vg    = _vis.addGraph(GROUP_GRAPH, _g)
      _vg.addColumn(COL_MUTA, classOf[AnyRef])

      val procRenderer = new BoxRenderer[S](this) // new NuagesShapeRenderer(50)
      val edgeRenderer = new MyEdgeRenderer
      // edgeRenderer.setArrowHeadSize(8, 8)

      val rf = new DefaultRendererFactory(procRenderer)
      rf.add(new InGroupPredicate(GROUP_EDGES), edgeRenderer)
      // rf.add(new InGroupPredicate(AGGR_PROC  ), aggrRenderer)
      _vis.setRendererFactory(rf)

      _lay = new ForceDirectedLayout(GROUP_GRAPH)
      val sim = new ForceSimulator
      sim.addForce(new NBodyForce)
      sim.addForce(new MySpringForce)
      sim.addForce(new DragForce)
      _lay.setForceSimulator(sim)

//      val forceMap = Map(
//        ("NBodyForce" , "GravitationalConstant") -> -2.0f,
//        ("DragForce"  , "DragCoefficient"      ) -> 0.002f,
//        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
//        ("SpringForce", "DefaultSpringLength"  ) -> 200.0f
//      )

//      val forceMap = Map(
//        ("NBodyForce" , "GravitationalConstant") -> -10.0f,
//        ("NBodyForce" , "Distance"             ) -> -1.0f,
//        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.57f,
//        ("DragForce"  , "DragCoefficient"      ) -> 0.023f,
//        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
//        ("SpringForce", "DefaultSpringLength"  ) -> 200.0f
//      )

//      val forceMap = Map(
//        ("NBodyForce" , "GravitationalConstant") -> -2.0f,
//        ("NBodyForce" , "Distance"             ) -> -1.0f,
//        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.57f,
//        ("DragForce"  , "DragCoefficient"      ) -> 0.01f,
//        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
//        ("SpringForce", "DefaultSpringLength"  ) -> 10.0f
//      )

      val forceMap = Map(
        ("NBodyForce" , "GravitationalConstant") -> -5.0f,
        ("NBodyForce" , "Distance"             ) -> -1.0f,
        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.4f,
        ("DragForce"  , "DragCoefficient"      ) -> 0.02f,
        ("MySpringForce", "SpringCoefficient"  ) -> 8.0e-5f,
        ("MySpringForce", "DefaultSpringLength") -> 150.0f,
        ("MySpringForce", "Torque"             ) -> 2.0e-4f,
        ("MySpringForce", "Limit"              ) -> 300.0f
      )

      forceSimulator.getForces.foreach { force =>
        val fName = force.getClass.getSimpleName
        // println(s"----FORCE----$fName")
        for (i <- 0 until force.getParameterCount) {
          val pName = force.getParameterName(i)
          forceMap.get((fName, pName)).foreach { value =>
            force.setParameter(i, value)
          }
          // println(pName)

          // NBodyForce
          // - GravitationalConstant = -2.0
          // - Distance = -1.0
          // - BarnesHutTheta = 0.89
          // DragForce
          // - DragCoefficient = 0.002
          // SpringForce
          // - SpringCoefficient = 1.0e-5
          // - DefaultSpringLength = 200.0
        }
      }

      _lay.setMaxTimeStep(40L)
      _lay.setIterations(1)
      // forceSimulator.setSpeedLimit(0.025f)

      // ------------------------------------------------

      // initialize the display
      _dsp.setSize(VIDEO_WIDTH_SQR, VIDEO_HEIGHT)
      _dsp.addControlListener(new ZoomControl     ())
      _dsp.addControlListener(new WheelZoomControl())
      _dsp.addControlListener(new PanControl        )
      _dsp.addControlListener(new DragControl       )
      _dsp.setHighQuality(true)

      // ------------------------------------------------

      edgeRenderer.setHorizontalAlignment1(Constants.CENTER)
      edgeRenderer.setHorizontalAlignment2(Constants.CENTER)
      edgeRenderer.setVerticalAlignment1  (Constants.CENTER)
      edgeRenderer.setVerticalAlignment2  (Constants.CENTER)

      _dsp.setForeground(Color.WHITE)
      _dsp.setBackground(Color.BLACK)

      //      setLayout( new BorderLayout() )
      //      add( display, BorderLayout.CENTER )
      val p = new JPanel
      p.setLayout(new Layout(_dsp))
      p.add(_dsp)

      mkAnimation()
      // _vis.run(ACTION_COLOR)

      component = Component.wrap(p)
    }

    def runAnimation: Boolean = _runAnim
    def runAnimation_=(value: Boolean): Unit = if (_runAnim != value) {
      requireEDT()
      _vis.synchronized {
        stopAnimation()
        _vis.removeAction(ACTION_COLOR)
        _vis.removeAction(ACTION_LAYOUT)
        _runAnim = value
        mkAnimation()
      }
    }

    private def mkAnimation(): Unit = {
      mkActionColor() // create really new instance, because `alwaysRunsAfter` installs listener on this
      val actionLayout = if (_runAnim) {
          new ActionList(Activity.INFINITY, LAYOUT_TIME)
        } else {
          new ActionList()
        }
      actionLayout.add(_lay)
      // actionLayout.add(new PrefuseAggregateLayout(AGGR_PROC))
      actionLayout.add(new RepaintAction())
      actionLayout.setVisualization(_vis)
      _vis.putAction(ACTION_LAYOUT, actionLayout)
      _vis.alwaysRunAfter(ACTION_COLOR, ACTION_LAYOUT)
      startAnimation()
    }

    def animationStep(): Unit = {
      requireEDT()
      _vis.synchronized {
        startAnimation()
      }
    }

    def saveFrameAsPNG(file: File): Unit = saveFrameAsPNG(file, width = _dsp.getWidth, height = _dsp.getHeight)

    def saveFrameAsPNG(file: File, width: Int, height: Int): Unit = {
      requireEDT()
      val bImg  = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val g     = bImg.createGraphics()
      try {
        _dsp.damageReport() // force complete redrawing
        _dsp.paintDisplay(g, new Dimension(width, height))
        ImageIO.write(bImg, "png", file)
      } finally {
        g.dispose()
      }
    }

    private def execOnEDT[A](code: => A)(implicit exec: ExecutionContext): A = {
      val p = Promise[A]()
      Swing.onEDT {
        val res = Try(code)
        p.complete(res)
      }
      blocking(Await.result(p.future, Duration.Inf)) //  Duration(20, TimeUnit.SECONDS)))
    }

    def saveFrameSeriesAsPNG(settings: VideoSettings): Processor[Unit] = {
      import settings._

      import ExecutionContext.Implicits.global
      val numVersions = cursorPos.single.get.size / 2

      def toFrames(sec: Double) = (sec * framesPerSecond + 0.5).toInt

      val framesPerIter = toFrames(secondsPerIteration)
      val framesDecay   = toFrames(secondsDecay)
      val framesSkip    = toFrames(secondsSkip)
      val numFrames   = numVersions * framesPerIter + framesDecay
      runAnimation    = false
      val child       = baseFile.base
      val parent      = baseFile.parent

      forceSimulator.setSpeedLimit(speedLimit.toFloat)


      //      lazy val bImg  = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      //      lazy val g     = bImg.createGraphics()

      var initialized = false

      Processor[Unit]("saveFrameSeriesAsPNG") { p =>
        var frame = 0

        // def mkF() = parent / f"$child${frame - framesSkip}%05d.png"
        def mkF() = parent / f"$child${frame - framesSkip}%d.png"

        while (frame < numFrames) {
          if (frame % framesPerIter == 0) /* execOnEDT */ {
            if (initialized) {
              previousIteration()
            } else {
              initChromosome(settings.chromosomeIndex)
              initialized = true
            }
          }

          val frameSave = frame - framesSkip
          if (frameSave >= 0) {
            val f = mkF()
            execOnEDT {
              saveFrameAsPNG(f, width = width, height = height)
              // _dsp.damageReport() // force complete redrawing
              // _dsp.paintDisplay(g, new Dimension(width, height))
              // ImageIO.write(bImg, "png", f)
            }
          }
          execOnEDT {
            animationStep()
          }
          frame += 1
          println(s"frame $frame")
          p.progress = frame.toDouble / numFrames
          p.checkAborted()
        }

        // now dissolve graph
        val m0 = map.snapshot.values
        println(s"VERTICES AT END: ${m0.size}")
        if (m0.nonEmpty) {
          import kollflitz.RandomOps._
          implicit val rnd = new util.Random(0L)
          val m = m0.toIndexedSeq.scramble()
          val framesPlop = toFrames(plopDur)
          // println(s"FRAMES-PLOP $framesPlop")
          @tailrec def loop(sq: Vec[VisualVertex[S]]): Unit = sq match {
            case head +: tail =>
              // println(s"DISPOSE $head")
              algorithm.global.cursor.step { implicit tx =>
                head.dispose()
              }
              for (i <- 1 to framesPlop) {
                val f = mkF()
                execOnEDT {
                  saveFrameAsPNG(f, width = width, height = height)
                }
                execOnEDT {
                  animationStep()
                }
                frame += 1
              }
              loop(tail)

            case _ =>
          }
          loop(m)
        }

        // execOnEDT(g.dispose())
      }
    }
  }

  private class Layout(peer: javax.swing.JComponent) extends LayoutManager {
    def layoutContainer(parent: java.awt.Container): Unit =
      peer.setBounds(new Rectangle(0, 0, parent.getWidth, parent.getHeight))

    def minimumLayoutSize  (parent: java.awt.Container): Dimension = peer.getMinimumSize
    def preferredLayoutSize(parent: java.awt.Container): Dimension = peer.getPreferredSize

    def removeLayoutComponent(comp: java.awt.Component) = ()

    def addLayoutComponent(name: String, comp: java.awt.Component) = ()
  }
}
trait Visual[S <: Sys[S]] extends VisualLike[S] {
  def algorithm: Algorithm[S]

  def forceSimulator: ForceSimulator

  def initChromosome(idx: Int): Unit
}