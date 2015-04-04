package de.sciss.mutagentx

import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm.{TxnLike, Identifiable}
import de.sciss.serial.{DataOutput, DataInput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object Genome {
  def empty(implicit tx: S#Tx): Genome = new Genome {
    def chromosomes = Vector.empty
  }

  def apply(size: Int)(implicit tx: S#Tx, r: TxnRandom.Persistent[D]): Genome = new Genome {
    val chromosomes   = Vector.fill(size)(ChromosomeH(8)(tx, r))
    // private val rngH  = tx.durable.newHandle(r)
    // def rng(implicit tx: D#Tx): TxnRandom[D#Tx] = rngH()
  }

  implicit object Ser extends Serializer[S#Tx, S#Acc, Genome] {
    def write(g: Genome, out: DataOutput): Unit = {
      ??? // Serializer.indexedSeq[D#Tx, D#Acc, ChromosomeH](ChromosomeH.Ser).write(g.chromosomes, out)
      // g.rng.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Genome = new Genome {
      val chromosomes   = Serializer.indexedSeq[S#Tx, S#Acc, ChromosomeH](ChromosomeH.Ser).read(in, access)
      ??? // private val rngH  = TxnRandom.Persistent.read[D]()
    }
  }
}
trait Genome {
  // def chromosomes: SkipList.Set[S, Chromosome]
  def chromosomes: Vec[ChromosomeH]
  // def rng(implicit tx: D#Tx): TxnRandom.Persistent[D]
}

object ChromosomeH {
  def apply(numBits: Int)(implicit tx: S#Tx, r: TxnRandom[D#Tx]): ChromosomeH = new ChromosomeH {
    val id      = tx.newID()
    val cursor  = tx.system.newCursor()
    val apply   = tx.newVar(id, Chromosome(numBits))
    val fitness = 0.0 // XXX
  }

  implicit object Ser extends Serializer[S#Tx, S#Acc, ChromosomeH] {
    def write(c: ChromosomeH, out: DataOutput): Unit = {
      c.id    .write(out)
      c.cursor.write(out)
      c.apply .write(out)
      out.writeDouble(c.fitness)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ChromosomeH = {
      implicit val dtx = tx.durable
      implicit val sys = tx.system
      new ChromosomeH {
        val id      = tx.readID(in, access)
        val cursor  = confluent.Cursor.read[S, D](in)
        val apply   = tx.readVar[Chromosome](id, in)
        val fitness = in.readDouble()
      }
    }
  }
}
trait ChromosomeH extends Identifiable[S#ID] {
  def cursor: confluent.Cursor[S, D]
  def apply: S#Var[Chromosome]
  def fitness: Double
}