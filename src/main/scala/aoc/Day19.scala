package aoc

import aoc.util.Intcode

object Day19 {

  implicit class Crossable[X](xs: Iterable[X]) {
    def cross[Y](ys: Iterable[Y]): Iterable[(X, Y)] = for {x <- xs; y <- ys} yield (x, y)
  }

  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    ((0L until 50) cross (0L until 50)).count(p => isPulled(p, program))
  }

  private def isPulled(pos: (Long, Long), program: Array[Long]): Boolean = {
    val computer = new Intcode(program)
    computer.execute(Seq(pos._1, pos._2).iterator)
    computer.output.last == 1
  }

  def second(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)

    def pulled(x: Long, y: Long): Boolean = isPulled((x, y), program)

    var (x1, y1) = (0L, 0L)
    var (x2, y2) = (0L, 0L)
    val santaSize = 100L

    def width(): Long = x2 - x1 + 1

    def height(): Long = y1 - y2 + 1

    while (width < santaSize || height < santaSize) {
      while (height < santaSize) {
        y1 += santaSize - height
        while (!pulled(x1, y1))
          x1 += 1
      }
      while (width < santaSize) {
        x2 += santaSize - width
        while (!pulled(x2, y2))
          y2 += 1
      }
    }

    x1 * 10000 + y2
  }
}
