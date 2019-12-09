package aoc

import aoc.util.Intcode

object Day05 {
  def second(program: String): Long = {
    execute(program, 5).head
  }

  private def execute(program: String, system: Long): Array[Long] = {
    val cells = program.split(',').map(_.toLong)
    val input = Seq(system)
    val computer = new Intcode(cells)
    computer.execute(input.iterator)
    computer.output
  }

  def first(program: String): Long = {
    execute(program, 1).find(v => v > 0).get
  }
}
