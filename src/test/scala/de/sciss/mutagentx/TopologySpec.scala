package de.sciss.mutagentx

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Mutable, MutableSerializer}
import de.sciss.mutagentx.{Topology => TopologyC}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.Confluent
import de.sciss.topology.{Topology => TopologyI}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

/*

test-only de.sciss.mutagentx.TopologySpec

 */
class TopologySpec extends FlatSpec with Matchers {
  case class VI(name: String) { override def productPrefix = "V" }
  case class EI(sourceVertex: VI, targetVertex: VI) extends TopologyI.Edge[VI] {
    override def productPrefix = "E"
  }

  type S = Confluent

  object VC {
    implicit object Ser extends MutableSerializer[S, VC] {
      protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): VC = {
        val name = in.readUTF()
        new VC(id, name)
      }
    }

    def apply(name: String)(implicit tx: S#Tx): VC = new VC(tx.newID(), name)
  }
  final class VC(val id: S#ID, val name: String) extends Mutable.Impl[S] {
    override def toString() = s"V($name)"

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def writeData(out: DataOutput): Unit = out.writeUTF(name)
  }
  object EC {
    implicit object Ser extends Serializer[S#Tx, S#Acc, EC] {
      def write(e: EC, out: DataOutput): Unit = {
        e.sourceVertex.write(out)
        e.targetVertex.write(out)
      }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): EC = {
        val sourceVertex = VC.Ser.read(in, access)
        val targetVertex = VC.Ser.read(in, access)
        EC(sourceVertex, targetVertex)
      }
    }
  }
  case class EC(sourceVertex: VC, targetVertex: VC) extends TopologyC.Edge[VC] {
    override def productPrefix = "E"
  }

  "A transactional topology" should "behave like the immutable counter-part" in {
    val ti0 = TopologyI.empty[VI, EI]
    val ti1 = ti0.addVertex(VI("a"))
    val ti2 = ti1.addVertex(VI("b"))
    val ti3 = ti2.addVertex(VI("c"))
    val ti4 = ti3.addVertex(VI("d"))
    val Success((ti5, mi0)) = ti4.addEdge(EI(VI("a"), VI("c")))
    val Success((ti6, mi1)) = ti5.addEdge(EI(VI("b"), VI("a")))
    val Success((ti7, mi2)) = ti6.addEdge(EI(VI("d"), VI("a")))
    val Failure(_)          = ti7.addEdge(EI(VI("c"), VI("d")))
    val ci0  = ti7.canAddEdge(EI(VI("c"), VI("b")))
    val ci1  = ti7.canAddEdge(EI(VI("b"), VI("d")))
    assert(ci0 === false)
    assert(ci1 === true )
    val si0   = ti7.vertices.mkString(",")
    val si1   = ti7.edges.toList.sortBy(e => e.sourceVertex.name -> e.targetVertex.name).mkString(",")
    val ti8   = ti7.removeVertex(VI("a"))
    val si2   = ti8.vertices.mkString(",")
    val si3   = ti8.edges.toList.sortBy(e => e.sourceVertex.name -> e.targetVertex.name).mkString(",")

    val system      = Confluent(BerkeleyDB.tmp())
    val (_, cursor) = system.cursorRoot(_ => ()) { implicit tx => _ => system.newCursor() }

    val (mc0, mc1, mc2, sc0, sc1, sc2, sc3) = cursor.step { implicit tx =>
      val tc = TopologyC.empty[VC, EC]
      val vca = VC("a")
      val vcb = VC("b")
      val vcc = VC("c")
      val vcd = VC("d")
      tc.addVertex(vca)
      tc.addVertex(vcb)
      tc.addVertex(vcc)
      tc.addVertex(vcd)
      val Success(_mc0) = tc.addEdge(EC(vca, vcc))
      val Success(_mc1) = tc.addEdge(EC(vcb, vca))
      val Success(_mc2) = tc.addEdge(EC(vcd, vca))
      val Failure(_)    = tc.addEdge(EC(vcc, vcd))
      val cc0  = tc.canAddEdge(EC(vcc, vcb))
      val cc1  = tc.canAddEdge(EC(vcb, vcd))
      assert(cc0 === false)
      assert(cc1 === true )
      val _sc0 = tc.vertices.iterator.toList.mkString(",")
      val _sc1 = tc.edges.iterator.toList.sortBy(e => e.sourceVertex.name -> e.targetVertex.name).mkString(",")

      assert(tc.edgeMap.get(vca.id).fold(0)(_.size) === 1)
      assert(tc.edgeMap.get(vcb.id).fold(0)(_.size) === 1)
      assert(tc.edgeMap.get(vcd.id).fold(0)(_.size) === 1)

      tc.removeVertex(vca)
      assert(tc.edgeMap.contains(vca.id) === false)

      val _sc2 = tc.vertices.iterator.toList.mkString(",")
      val _sc3 = tc.edges.iterator.toList.sortBy(e => e.sourceVertex.name -> e.targetVertex.name).mkString(",")

      (_mc0, _mc1, _mc2, _sc0, _sc1, _sc2, _sc3)
    }

    assert(mi0.mkString(",") === mc0.mkString(","))
    assert(mi1.mkString(",") === mc1.mkString(","))
    assert(mi2.mkString(",") === mc2.mkString(","))

    assert(si0 === sc0)
    assert(si1 === sc1)
    assert(si2 === sc2)
    assert(si3 === sc3)
  }
}
