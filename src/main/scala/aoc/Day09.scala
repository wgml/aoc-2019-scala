package aoc

import aoc.util.Intcode

object Day09 {
  def first(program: String): Long = {
    execute(program, 1)
  }

  def second(program: String): Long = {
    execute(program, 2)
  }

  private def execute(programStr: String, mode: Long) = {
    val program = programStr.split(',').map(_.toLong)
    val computer = new Intcode(program)
    val input = Seq(mode)
    computer.execute(input.iterator)
    computer.output.head
  }
}
