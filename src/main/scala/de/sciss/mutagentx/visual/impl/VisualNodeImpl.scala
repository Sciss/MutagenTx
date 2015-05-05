package de.sciss.mutagentx
package visual
package impl

import java.awt.geom.{AffineTransform, GeneralPath, Ellipse2D, Rectangle2D, Line2D, Arc2D}
import java.awt.{Font, Shape, BasicStroke, Color}

import prefuse.data.{Node => PNode}
import prefuse.util.ColorLib
import prefuse.visual.VisualItem

import scala.concurrent.stm.{Ref, TMap}
import scala.swing.Graphics2D

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
private trait VisualNodeImpl extends VisualNode with VisualDataImpl {
  import VisualNodeImpl._

  private[this] var _pNode: PNode = _

  val edgesIn  = TMap.empty[(VisualNode, VisualNode), VisualEdge]
  val edgesOut = TMap.empty[(VisualNode, VisualNode), VisualEdge]

  def dispose()(implicit tx: S#Tx): Unit = {
    implicit val itx = tx.peer
    edgesIn .foreach(_._2.dispose())
    edgesOut.foreach(_._2.dispose())
    main.deferVisTx {
      if (pNode.isValid) main.graph.removeNode(pNode)
    }
  }

  final def pNode: PNode = {
    if (_pNode == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
    _pNode
  }

  private[this] val _init = Ref(initialValue = false)

  def init()(implicit tx: S#Tx): Unit = {
    implicit val itx = tx.peer
    require(!_init.swap(true), s"Already initialized: $this")

    touch()
    main.deferVisTx {
      mkPNode()
      if (VisualOLD.DEBUG) println(s"MAKE NODE $this")
    }
  }

  private def mkPNode(): Unit = {
    if (_pNode != null) throw new IllegalStateException(s"Component $this has already been initialized")
    _pNode  = main.graph.addNode()
    val vis = main.visualization
    val vi  = vis.getVisualItem(VisualOLD.GROUP_GRAPH, _pNode)
    vi.set(VisualOLD.COL_MUTA, this)
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
      _font = VisualOLD.condensedFont.deriveFont(fontSize)
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
