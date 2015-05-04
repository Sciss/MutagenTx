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
import java.util.concurrent.TimeUnit
import javax.imageio.ImageIO
import javax.swing.JPanel

import de.sciss.file._
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, defer, deferTx, requireEDT}
import de.sciss.processor.Processor
import prefuse.action.assignment.ColorAction
import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.action.{ActionList, RepaintAction}
import prefuse.activity.Activity
import prefuse.controls.{DragControl, PanControl, WheelZoomControl, ZoomControl}
import prefuse.data.{Graph => PGraph}
import prefuse.render.{DefaultRendererFactory, EdgeRenderer}
import prefuse.util.ColorLib
import prefuse.visual.expression.InGroupPredicate
import prefuse.visual.{VisualGraph, VisualItem}
import prefuse.{Constants, Display, Visualization}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise, ExecutionContext, blocking}
import scala.concurrent.stm.{Ref, TMap, TxnLocal}
import scala.swing.{Swing, Component, Dimension, Graphics2D, Rectangle}
import scala.util.Try
import scala.util.control.NonFatal

object VisualOLD {
  val DEBUG = false

  val VIDEO_WIDTH     = 720
  val VIDEO_HEIGHT    = 576
  val VIDEO_WIDTH_SQR = 1024 // 1024 : 576 = 16 : 9

  def apply(a: AlgorithmOLD)(implicit tx: S#Tx): VisualOLD = {
    val map = TMap.empty[S#ID, VisualBit]
    implicit val dtx = tx.durable
    new Impl(map, a, a.global.cursor.position).init()
  }

  private lazy val _initFont: Font = {
//    val is  = Wolkenpumpe.getClass.getResourceAsStream("BellySansCondensed.ttf")
//    val res = Font.createFont(Font.TRUETYPE_FONT, is)
//    is.close()
    val res = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
    res
    //      // "SF Movie Poster Condensed"
    //      new Font( "BellySansCondensed", Font.PLAIN, 12 )
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

  private final class Impl(map: TMap[S#ID, VisualBit], val algorithm: AlgorithmOLD, cursorPos0: S#Acc)
    extends VisualOLD with ComponentHolder[Component] {

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
      val c = algorithm.genome.chromosomes().head
      // val numBits = c.size
      insertChromosome(c)
      this
    }

    private def insertChromosome(c: ChromosomeOLD)(implicit tx: S#Tx): Unit = {
      def loop(pred: Option[Bit], curr: Option[Bit]): Unit = {
        curr.foreach { b =>
          checkOrInsertBit(b)
          pred.foreach { p =>
            checkOrInsertLink(p, b)
          }
          loop(pred = curr, curr = b.next())
        }
      }

      //      val csz = c.size
      //      if (csz != 32) {
      //        println(s"WARNING = $c size is $csz")
      //      }

      loop(None, c.head())
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
      _vis.cancel(ACTION_COLOR)
      _vis.cancel(ACTION_LAYOUT)
    }

    @inline private def startAnimation(): Unit =
      _vis.run(ACTION_COLOR)

    private def checkOrInsertBit(b: Bit)(implicit tx: S#Tx): Unit =
      map.get(b.id)(tx.peer).fold {
        insertBit(b)
      } { v =>
        val state = b.bit()
        if (state != v.state) {
          v.state = state // XXX animate -- how? THREADING
        }
        v.touch()
      }

    private def insertBit(b: Bit)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val v = VisualBit(this, b)
      val res = map.put(b.id, v)
      require(res.isEmpty)
      deferVisTx(insertBitGUI(v, locO = None))
    }

    private def initNodeGUI(obj: VisualBit, vn: VisualBit, locO: Option[Point2D]): VisualItem = {
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

    private def insertBitGUI(v: VisualBit, locO: Option[Point2D]): Unit = {
      initNodeGUI(v, v, locO)
    }

    private def checkOrInsertLink(pred: Bit, succ: Bit)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val tup = for {
        predV <- map.get(pred.id)
        succV <- map.get(succ.id)
      } yield (predV, succV)

      if (tup.isEmpty) {
        println(s"WARNING: link misses vertices: $pred / $succ")
      }

      tup.foreach { case (predV, succV) =>
        val edge = VisualEdge(predV, succV, init = false)
        predV.edgesOut.get(edge.key).fold[Unit](edge.init())(_.touch())
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()

    private def shrink(in: S#Acc): S#Acc = {
      val prevIdx = in.take(in.size - 3)
      prevIdx :+ prevIdx.term
    }

    def previousIteration(): Unit = {
      val pos = cursorPos.single.transformAndGet(c => c.take(c.size - 2))
      if (pos.isEmpty) return

      algorithm.global.forkCursor.stepFrom(pos) { implicit tx =>
        implicit val itx = tx.peer
        val mapOld = map.snapshot
        map.clear()
        var toRemove = Set.empty[VisualData]
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
        val cs = algorithm.genome.chromosomes()
        val ancestors = cs.filter { c =>
          c.exists { b =>
            val bid = b.id
            map.contains(bid)
          }
        }
        println(s"Num-ancestors = ${ancestors.size}")
        ancestors.foreach(insertChromosome)
      // }

        map.foreach { case (_, v) =>
          if (!v.isActive) toRemove += v

          def checkEdges(es: TMap[_, VisualEdge]): Unit = es.foreach {
            case (_, e) => if (!e.isActive) toRemove += e
          }

          checkEdges(v.edgesIn)
          checkEdges(v.edgesOut)
        }

        if (DEBUG) {
          val numVertices = toRemove.count(_.isInstanceOf[VisualBit])
          val numEdges    = toRemove.count(_.isInstanceOf[VisualEdge])
          println(s"map.size = ${mapOld.size} -> ${map.size}; toRemove.size = $numVertices vertices / $numEdges edges")
        }

        toRemove.foreach(_.dispose())
      }
    }

    private def mkActionColor(): Unit = {
      // colors
      val actionNodeStroke  = new ColorAction(GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      actionNodeStroke.add(VisualItem.HIGHLIGHT, ColorLib.rgb(255, 255, 0))
      // actionNodeStroke.add(VisualItem.HIGHLIGHT, ColorLib.rgb(220, 220, 0))
      val actionNodeFill    = new ColorAction(GROUP_NODES, VisualItem.FILLCOLOR  , ColorLib.rgb(0, 0, 0))
      actionNodeFill.add(VisualItem.HIGHLIGHT, ColorLib.rgb(63, 63, 0))
      val actionTextColor   = new ColorAction(GROUP_NODES, VisualItem.TEXTCOLOR  , ColorLib.rgb(255, 255, 255))

      val actionEdgeColor   = new ColorAction(GROUP_EDGES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      //      val actionAggrFill    = new ColorAction(AGGR_PROC  , VisualItem.FILLCOLOR  , ColorLib.rgb(80, 80, 80))
      //      val actionAggrStroke  = new ColorAction(AGGR_PROC  , VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))

      actionColor = new ActionList(_vis)
      actionColor.add(actionTextColor)
      actionColor.add(actionNodeStroke)
      actionColor.add(actionNodeFill)
      actionColor.add(actionEdgeColor)
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

      _g     = new PGraph
      _vg    = _vis.addGraph(GROUP_GRAPH, _g)
      _vg.addColumn(COL_MUTA, classOf[AnyRef])

      val procRenderer = new NuagesShapeRenderer(50)
      val edgeRenderer = new EdgeRenderer(Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_FORWARD)

      val rf = new DefaultRendererFactory(procRenderer)
      rf.add(new InGroupPredicate(GROUP_EDGES), edgeRenderer)
      // rf.add(new InGroupPredicate(AGGR_PROC  ), aggrRenderer)
      _vis.setRendererFactory(rf)

      _lay = new ForceDirectedLayout(GROUP_GRAPH)

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

    private def saveFrameAsPNG(file: File, width: Int, height: Int): Unit = {
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
      blocking(Await.result(p.future, Duration(5, TimeUnit.SECONDS)))
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
      Processor[Unit]("saveFrameSeriesAsPNG") { p =>
        var frame = 0
        while (frame < numFrames) {
          val frameSave = frame - framesSkip
          if (frameSave >= 0) {
            val f = parent / f"$child$frameSave%05d.png"
            execOnEDT {
              saveFrameAsPNG(f, width = width, height = height)
            }
          }
          execOnEDT {
            animationStep()
          }
          frame += 1
          println(frame)
          p.progress = frame.toDouble / numFrames
          p.checkAborted()
          if (frame % framesPerIter == 0) execOnEDT {
            previousIteration()
          }
        }
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
trait VisualOLD extends VisualLike {
  def algorithm: AlgorithmOLD
}