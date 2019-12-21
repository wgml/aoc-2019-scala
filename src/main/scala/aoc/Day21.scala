package aoc

import aoc.util.Intcode

object Day21 {

  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    val code = Seq(
      "NOT C J",
      "NOT A T",
      "OR T J",
      "AND D J",
      "WALK")
    compileAndRun(program, code)
  }

  private def compileAndRun(program: Array[Long], code: Seq[String]): Long = {
    val compiled = code.reduce(_ + '\n' + _) + '\n'
    val computer = new Intcode(program)
    computer.execute(compiled.map(_.toLong).iterator)
    computer.output.last
  }

  def second(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    val code = Seq(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "OR E T",
      "OR H T",
      "AND T J",
      "RUN"
    )
    compileAndRun(program, code)
  }
}
