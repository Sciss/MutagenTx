/*
 *  Genome
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

import de.sciss.lucre.confluent
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

import scala.collection.immutable.{IndexedSeq => Vec}

object Genome {
  def empty(implicit tx: S#Tx): Genome = {
    val id          = tx.newID()
    val chromosomes = tx.newVar(id, Vec.empty[Chromosome])
    val cursor      = tx.system.newCursor()
    new GenomeImpl(id, chromosomes, cursor)
  }

  //  def apply(size: Int)(implicit tx: S#Tx, r: TxnRandom.Persistent[D]): Genome = new Genome {
  //    val id            = tx.newID()
  //    val chromosomes   = Vector.fill(size)(ChromosomeH(8)(tx, r))
  //    // private val rngH  = tx.durable.newHandle(r)
  //    // def rng(implicit tx: D#Tx): TxnRandom[D#Tx] = rngH()
  //  }

  private final class GenomeImpl(val id: S#ID, val chromosomes: S#Var[Vec[Chromosome]],
                                 val cursor: confluent.Cursor[S, D])
    extends Genome {

    def write(out: DataOutput): Unit = {
      id          .write(out)
      chromosomes .write(out)
      cursor      .write(out)
    }
  }

  implicit object Ser extends Serializer[S#Tx, S#Acc, Genome] {
    def write(g: Genome, out: DataOutput): Unit = g.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Genome = {
      val id            = tx.readID(in, access)
      val chromosomes   = tx.readVar[Vec[Chromosome]](id, in)
      val cursor        = tx.system.readCursor(in)
      new GenomeImpl(id, chromosomes, cursor)
    }
  }
}
trait Genome extends Writable {
  def chromosomes: S#Var[Vec[Chromosome]]

  def cursor: confluent.Cursor[S, D]
}