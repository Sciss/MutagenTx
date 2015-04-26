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

import java.awt.geom.{AffineTransform, Arc2D, Line2D, GeneralPath, Ellipse2D, Rectangle2D, Point2D}
import java.awt.{Font, BasicStroke, Shape, LayoutManager, Color, RenderingHints}
import javax.swing.JPanel

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.swing.{View, defer, deferTx, requireEDT}
import de.sciss.lucre.swing.impl.ComponentHolder
import prefuse.action.{RepaintAction, ActionList}
import prefuse.action.assignment.ColorAction
import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.activity.Activity
import prefuse.controls.{DragControl, PanControl, WheelZoomControl, ZoomControl}
import prefuse.data.{Node => PNode, Graph => PGraph}
import prefuse.render.{EdgeRenderer, DefaultRendererFactory}
import prefuse.util.ColorLib
import prefuse.visual.{VisualItem, VisualGraph}
import prefuse.visual.expression.InGroupPredicate
import prefuse.{Constants, Display, Visualization}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{TSet, TMap, Ref, TxnLocal}
import scala.swing.{Dimension, Rectangle, Graphics2D, Component}
import scala.util.control.NonFatal

object Visual {
  def apply(a: Algorithm)(implicit tx: S#Tx): Visual = {
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

  private object VisualNodeImpl {
    final val diam    = 50
    final val diam05  = 25 // diam * 0.5

    private final val eps = 1.0e-2

    final val colrPlaying   = new Color(0x00, 0xC0, 0x00)
    final val colrStopped   = new Color(0x80, 0x80, 0x80)
    final val colrBypassed  = new Color(0xFF, 0xC0, 0x00)
    final val colrSoloed    = new Color(0xFF, 0xFF, 0x00)
    final val colrMapped    = new Color(210, 60, 60)
    final val colrManual    = new Color(60, 60, 240)
    final val colrGliding   = new Color(135, 60, 150)
    final val colrAdjust    = new Color(0xFF, 0xC0, 0x00)

    final val strkThick     = new BasicStroke(2f)
    final val strkVeryThick = new BasicStroke(4f)
    final val strkDotted    = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(1f, 1f), 0f)

    final val gArc          = new Arc2D.Double
    final val gLine         = new Line2D.Double

    final val margin        = diam * 0.2
    final val margin2       = margin * 2

    // final val threeDigits   = new MathContext(3, RoundingMode.HALF_UP)
  }
  private trait VisualNodeImpl extends VisualNode {
    import VisualNodeImpl._

    private[this] var _pNode: PNode = _

    val edgesIn  = TSet.empty[VisualEdge]
    val edgesOut = TSet.empty[VisualEdge]

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      edgesIn .foreach(_.dispose())
      edgesOut.foreach(_.dispose())
    }

    final def pNode: PNode = {
      if (_pNode == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
      _pNode
    }

    protected final def mkPNode(): Unit = {
      if (_pNode != null) throw new IllegalStateException(s"Component $this has already been initialized")
      _pNode  = main.graph.addNode()
      val vis = main.visualization
      val vi  = vis.getVisualItem(Visual.GROUP_GRAPH, _pNode)
      vi.set(Visual.COL_MUTA, this)
//      val sz  = nodeSize
//      if (sz != 1.0f) vi.set(VisualItem.SIZE, sz)
//      parent.aggr.addItem(vi)
    }

    protected val r: Rectangle2D = new Rectangle2D.Double()
    protected var outline: Shape = r
    protected val outerE  = new Ellipse2D.Double()
    protected val innerE  = new Ellipse2D.Double()
    protected val gp      = new GeneralPath()

    var fixed = false

    def update(shp: Shape): Unit = {
      val newR = shp.getBounds2D
      if ((math.abs(newR.getWidth  - r.getWidth ) < eps) &&
        (math.abs(newR.getHeight - r.getHeight) < eps)) {

        r.setFrame(newR.getX, newR.getY, r.getWidth, r.getHeight)
        return
      }
      r.setFrame(newR)
      outline = shp

      outerE.setFrame(0, 0, r.getWidth, r.getHeight)
      innerE.setFrame(margin, margin, r.getWidth - margin2, r.getHeight - margin2)
      gp.reset()
      gp.append(outerE, false)
      boundsResized()
    }

    private var _fontSize = 0f
    private var _font: Font = _

    def render(g: Graphics2D, vi: VisualItem): Unit = {
      // fixed nodes are indicated by a think white outline
      if (fixed) {
        val strkOrig = g.getStroke
        g.setStroke(strkVeryThick)
        g.setColor(ColorLib.getColor(vi.getStrokeColor))
        g.draw(outline)
        g.setStroke(strkOrig)
      }
      g.setColor(ColorLib.getColor(vi.getFillColor))
      g.fill(outline)
      val atOrig = g.getTransform
      g.translate(r.getX, r.getY)
      //         g.setRenderingHint( RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON )
      renderDetail(g, vi)
      g.setTransform(atOrig)
    }

    private[this] var lastFontT: AffineTransform = _
    private[this] var lastLabel: String = _
    private[this] var labelShape: Shape = _

    //    protected def drawName(g: Graphics2D, vi: VisualItem, fontSize: Float): Unit =
    //      drawLabel(g, vi, fontSize, name)

    protected def drawLabel(g: Graphics2D, vi: VisualItem, fontSize: Float, text: String): Unit = {
      if (_fontSize != fontSize) {
        _fontSize = fontSize
        _font = Visual.condensedFont.deriveFont(fontSize)
      }

      g.setColor(ColorLib.getColor(vi.getTextColor))

      if (main.display.isHighQuality) {
        val frc   = g.getFontRenderContext
        val frcT  = frc.getTransform
        if (frcT != lastFontT || text != lastLabel) {  // only calculate glyph vector if zoom level changes
        val v = _font.createGlyphVector(frc, text)
          // NOTE: there is a bug, at least with the BellySansCondensed font,
          // regarding `getVisualBounds`; it returns almost infinite width
          // for certain strings such as `"freq"`. Instead, using `getPixelBounds`
          // seems to resolve the issue.
          //
          // val vvb = v.getVisualBounds

          // NOTE: the getPixelBounds somehow incorporates wrong zoom factors.
          // The problem with `getVisualBounds` seems to originate from the
          // initial font-render-context.
          val vvb = if (frc.isTransformed) v.getVisualBounds else v.getPixelBounds(frc, 0f, 0f)

          // if (name == "freq") println(s"w = ${vvb.getWidth}, h = ${vvb.getHeight}; t? ${frc.isTransformed}")

          // for PDF output, drawGlyphVector gives correct font rendering,
          // while drawString only does with particular fonts.
          //         g.drawGlyphVector( v, ((r.getWidth() - vb.getWidth()) * 0.5).toFloat,
          //                           ((r.getHeight() + (fm.getAscent() - fm.getLeading())) * 0.5).toFloat )
          //         g.drawGlyphVector( v, ((r.getWidth() - vb.getWidth()) * 0.5).toFloat,
          //                               ((r.getHeight() - vb.getHeight()) * 0.5).toFloat )
          labelShape = v.getOutline(((r.getWidth - vvb.getWidth) * 0.5).toFloat,
            ((r.getHeight + vvb.getHeight) * 0.5).toFloat)
          lastFontT = frcT
          lastLabel = text
        }
        g.fill(labelShape)

      } else {
        val cx = r.getWidth  / 2
        val cy = r.getHeight / 2
        val fm = g.getFontMetrics
        g.drawString(text, (cx - (fm.stringWidth(text) * 0.5)).toInt, (cy + ((fm.getAscent - fm.getLeading) * 0.5)).toInt)
      }
    }

    protected def boundsResized(): Unit

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit
  }

  private object VisualBit {
    import VisualNodeImpl.diam

    def apply(_main: Visual, b: Bit)(implicit tx: S#Tx): VisualBit = new VisualBit with VisualNodeImpl {
      var state = b.bit()
      val main  = _main

      protected def boundsResized(): Unit = ()

      protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit =
        drawLabel(g, vi, diam * vi.getSize.toFloat * 0.5f, name)

      def name = if (state) "1" else "0"

      private def initGUI(): Unit = {
        requireEDT()
        mkPNode()
      }

      _main.deferVisTx(initGUI())
    }
  }
  private trait VisualBit extends VisualNode {
    var state: Boolean
  }

  private final class Impl(map: TMap[S#ID, VisualBit], algo: Algorithm, cursorPos0: S#Acc)
    extends Visual with ComponentHolder[Component] {

    private var _vis: Visualization = _
    private var _dsp: Display       = _
    private var _g  : PGraph        = _
    private var _vg : VisualGraph   = _

    private val cursorPos = Ref(cursorPos0)

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      val c = algo.genome.chromosomes().head
      // val numBits = c.size
      insertChromosome(c)
      this
    }

    private def insertChromosome(c: Chromosome)(implicit tx: S#Tx): Unit = {
      def loop(pred: Option[Bit], curr: Option[Bit]): Unit = {
        curr.foreach { b =>
          checkOrInsertBit(b)
          pred.foreach { p =>
            insertLink(p, b)
          }
          loop(pred = curr, curr = b.next())
        }
      }

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
          v.state = state // XXX animate -- how?
        }
      }

    private def insertBit(b: Bit)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val v = VisualBit(this, b)
      map.put(b.id, v)
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

    private def insertLink(pred: Bit, succ: Bit)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      for {
        predV <- map.get(pred.id)
        succV <- map.get(succ.id)
      } {
        val edge = VisualEdge(predV, succV, init = false)
        if (!predV.edgesOut.contains(edge)) edge.init()
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()

    private def shrink(in: S#Acc): S#Acc = {
      val prevIdx = in.take(in.size - 3)
      prevIdx :+ prevIdx.term
    }

    def previousIteration(): Unit = {
      val pos = cursorPos.single.transformAndGet(c => c.take(c.size - 2))

      algo.global.forkCursor.stepFrom(pos) { implicit tx =>
        implicit val itx = tx.peer
        val mapOld = map.snapshot
        map.clear()
        var toRemove = Set.empty[VisualBit]
        mapOld.foreach { case (idOld, v) =>
          val pathOld = idOld.path
          val pathNew = shrink(pathOld)
          if (pathNew.isEmpty) {
            toRemove += v
          } else {
            val idNew = idOld.copy(pathNew)
            map.put(idNew, v)
          }
        }

        val cs = algo.genome.chromosomes()
        val ancestors = cs.filter { c =>
          c.exists { b =>
            val bid = b.id
            map.contains(bid)
          }
        }
        ancestors.foreach(insertChromosome)

        if (toRemove.nonEmpty) deferVisTx {
          toRemove.foreach { v =>
            _g.removeNode(v.pNode)
          }
        }
      }
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

      val lay = new ForceDirectedLayout(GROUP_GRAPH)

      // quick repaint
      val actionColor = new ActionList()
      actionColor.add(actionTextColor)
      actionColor.add(actionNodeStroke)
      actionColor.add(actionNodeFill)
      actionColor.add(actionEdgeColor)
      //      actionColor.add(actionAggrFill)
      //      actionColor.add(actionAggrStroke)
      _vis.putAction(ACTION_COLOR, actionColor)

      val actionLayout = new ActionList(Activity.INFINITY, LAYOUT_TIME)
      actionLayout.add(lay)
      // actionLayout.add(new PrefuseAggregateLayout(AGGR_PROC))
      actionLayout.add(new RepaintAction())
      _vis.putAction(ACTION_LAYOUT, actionLayout)
      _vis.alwaysRunAfter(ACTION_COLOR, ACTION_LAYOUT)

      // ------------------------------------------------

      // initialize the display
      _dsp.setSize(960, 640)
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

      _vis.run(ACTION_COLOR)
      component = Component.wrap(p)
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
trait Visual extends View[S] {
  def display: Display

  def visualization: Visualization

  def graph: PGraph

  def visualGraph: VisualGraph

  /** Schedule code to be executed during paused visualization animation
    * on the EDT after the commit of the transaction.
    */
  def deferVisTx(thunk: => Unit)(implicit tx: TxnLike): Unit

  def previousIteration(): Unit
}