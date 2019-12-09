package aoc

import aoc.util.{Intcode, NoInput}

object Day07 {
  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    (0 to 4).permutations.map(perm => emulate(program, perm)).max
  }

  def emulate(program: Array[Long], phases: Seq[Int]): Long = {
    var signal = 0L
    for (phase <- phases) {
      val amplifier = new Intcode(program.clone)
      amplifier.execute(Seq(phase, signal).iterator)
      signal = amplifier.output.last
    }
    signal
  }

  def second(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    (5 to 9).permutations.map(perm => emulateWithFeedback(program, perm)).max
  }

  def emulateWithFeedback(program: Array[Long], phases: Seq[Int]): Long = {
    def executeOnce(amplifier: Intcode, input: Seq[Long]): Unit = {
      try {
        amplifier.execute(input.iterator, reset = false)
      } catch {
        case _: NoInput => ;
      }
    }

    val amplifiers = phases.map(_ => new Intcode(program.clone))
    (amplifiers zip phases).foreach(p => executeOnce(p._1, Seq(p._2)))

    var signal = 0L
    while (!amplifiers.last.halted) {
      for (amp <- amplifiers) {
        executeOnce(amp, Seq(signal))
        signal = amp.output.last
      }
    }
    signal
  }
}
