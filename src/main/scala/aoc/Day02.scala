package aoc

import aoc.util.Intcode

object Day02 {
  def second(program: String): Int = {
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        if (first(program, noun, verb) == 19690720)
          return 100 * noun + verb
      }
    }
    throw new Exception
  }

  def first(program: String, noun: Int = 12, verb: Int = 2): Long = {
    val cells = program.split(',').map(_.toLong)
    cells(1) = noun
    cells(2) = verb
    val computer = new Intcode(cells)
    computer.execute()
    computer.rawMemory(0)
  }
}
