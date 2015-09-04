package de.sciss.mutagentx

import de.sciss.lucre.stm.InMemory
import de.sciss.synth
import de.sciss.synth.SynthGraph
import org.scalatest.{FlatSpec, Matchers}

/*
  to run only this test

  test-only de.sciss.mutagentx.ConversionSpec

 */
class ConversionSpec extends FlatSpec with Matchers {
  "An example synth-graph" should "be convertible to chromosome and back without loss" in {

    // NB: not supported:
    // - multi-channel expansion other than Mix
    // - control-rate

    val g0 = SynthGraph {
      import synth._
      import ugen._
      // simulate what Chromosome does in expansion:
      RandSeed.ir()

      val f = 80       // fundamental frequency
      val p = 10       // number of partials per channel
      val trig = XLine.ar(10, 0.1, 60, doneAction = freeSelf) // trigger probability decreases over time
      val sig = Mix.tabulate(p) { i =>
        FSinOsc.ar(f * (i+1)) *    // freq of partial
          Decay2.ar(
            Dust.ar(trig) // trigger rate
              * 0.02,     // trigger amplitude
            0.005,        // grain attack time
            math.random * 0.5 // Rand(0,0.5)   // grain decay time
          )
      }
      // simulate what Chromsome does in expansion:
      val mono = Mix.mono(sig)
      ConfigOut(mono)
    }

    type S = InMemory
    val system = InMemory()

    val g1 = system.step { implicit tx =>
      val c0 = impl.ChromosomeImpl.mkChromosome(g0)
      impl.ChromosomeImpl.mkSynthGraph(c0, mono = true, removeNaNs = false, config = true)
    }

    val g0s = g0.sources.toSet
    val g1s = g1.sources.toSet

    val onlyInG0 = g0s -- g1s
    val onlyInG1 = g1s -- g0s

    if (onlyInG0.nonEmpty) {
      Console.err.println(s"\n-------- Only in g0 --------\n\n${onlyInG0.mkString("\n")}")
    }
    if (onlyInG1.nonEmpty) {
      Console.err.println(s"\n-------- Only in g1 --------\n\n${onlyInG1.mkString("\n")}")
    }

    assert(onlyInG0.isEmpty)
    assert(onlyInG1.isEmpty)
  }
}