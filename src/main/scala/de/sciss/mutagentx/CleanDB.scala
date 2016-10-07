/*
 *  CleanDB.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx

import de.sciss.file._
import de.sciss.lucre.{event => evt, stm}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object CleanDB extends App {
  import Algorithm.executionContext

  val dataDir   = file("database")
  val audioFile = file("audio_work") / "derrida1_1_216500.aif"
  val dbIn      = dataDir / "derrida1_1_216500"
  val dbOut     = dataDir / "derrida1_1_216500_CLEAN"
  require(!dbOut.exists)
  val inFut     =                      GeneratorApp.DurableInit(???, /* dir = dbIn , input = audioFile, */ init = false)
  val outFut    = inFut.flatMap { _ => GeneratorApp.DurableInit(???, /* dir = dbOut, input = audioFile, */ init = false) }

  val in        = Await.result(inFut , Duration.Inf)
  val out       = Await.result(outFut, Duration.Inf)

//  new Thread {
//    override def run(): Unit = this.synchronized(this.wait())
//    start()
//  }
//
//  inFut .onFailure { case ex => ex.printStackTrace( )}
//  outFut.onFailure { case ex => ex.printStackTrace( )}
//
//  for {
//    in  <- inFut
//    out <- outFut
//  } {
    // in.global.rng.getSeed() // XXX TODO - doesn't exist
    import in .global.{cursor => cs}
    import out.global.{cursor => ct}

    val (genomeIn, fit, numIter) = cs.step { implicit tx =>
      (in.genome.chromosomes(), in.genome.fitness(), in.global.numIterations())
    }
    import Algorithm.DurableVertexOrdering
    var lastProg = 0
    println("_" * 50)
    val genomeOut = genomeIn.zipWithIndex.map { case (c, idx) =>
      val res = impl.CopyingAlgorithm.mkCopyT[stm.Durable, stm.Durable](c, cs, ct)
      val prog = ((idx + 1) * 50) / genomeIn.size
      while (lastProg < prog) {
        print('#')
        lastProg += 1
      }
      res
    }
    // println()

    ct.step { implicit tx =>
      out.global.numIterations()  = numIter
      out.genome.chromosomes()    = genomeOut
      out.genome.fitness()        = fit
    }

    in .close()
    out.close()
    println(".")
    sys.exit()
//  }
}
