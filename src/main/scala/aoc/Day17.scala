package aoc

import aoc.util.Intcode

object Day17 {

  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    val computer = new Intcode(program)
    computer.execute(Seq().iterator)
    intersections(computer.output.map(_.toChar)).map(p => p._1 * p._2).sum
  }

  private def intersections(map: Array[Char]): Seq[(Int, Int)] = {
    val lines = map.mkString.split("\n")

    def isCross(x: Int, y: Int): Boolean =
      lines(y)(x) != '.' && lines(y)(x + 1) != '.' && lines(y)(x - 1) != '.' && lines(y - 1)(x) != '.' && lines(y + 1)(x) != '.'

    var result = Seq[(Int, Int)]()
    for (y <- 1 until lines.length - 2)
      for (x <- 1 until lines(y).length - 2)
        if (isCross(x, y))
          result = result :+ (x, y)
    result
  }

  def second(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)

    // read manually
    // "L,4,L,6,L,8,L,12,L,8,R,12,L,12,L,8,R,12,L,12,L,4,L,6,L,8,L,12,L,8,R,12,L,12,R,12,L,6,L,6,L,8,L,4,L,6,L,8,L,12,R,12,L,6,L,6,L,8,L,8,R,12,L,12,R,12,L,6,L,6,L,8"
    val A = "L,4,L,6,L,8,L,12"
    val B = "L,8,R,12,L,12"
    val C = "R,12,L,6,L,6,L,8"
    val routine = "A,B,B,A,B,C,A,C,B,C"
    val feed = "n"

    val code = Seq(routine, A, B, C, feed).reduce((a, b) => a + '\n' + b) + '\n'
    val compiled = code.map(_.toLong)

    val computer = new Intcode(program)
    computer.withMemoryOverride(0, 2)
    computer.execute(compiled.iterator)
    computer.output.last
  }
}
