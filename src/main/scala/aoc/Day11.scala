package aoc

import aoc.util.Intcode

object Day11 {

  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    val panels = paint(program, colors.black)
    panels.size
  }

  private def paint(program: Array[Long], startColor: Int): Map[(Long, Long), Long] = {
    def turn(dir: Int, mod: Long): Int = (dir - mod.toInt * 2 + 3) % 4

    def advance(pos: (Long, Long), dir: Int): (Long, Long) = {
      val (x, y) = pos
      dir match {
        case dirs.`up` => (x, y + 1)
        case dirs.`right` => (x + 1, y)
        case dirs.`down` => (x, y - 1)
        case dirs.`left` => (x - 1, y)
      }
    }

    val computer = new Intcode(program)
    var pos = (0L, 0L)
    var dir = dirs.up
    var panels: Map[(Long, Long), Long] = Map()
    panels += pos -> startColor

    while (!computer.halted) {
      val input: Seq[Long] = Seq(panels.getOrElse(pos, colors.black))
      computer.executeAndWaitForInput(input.iterator)

      val output = computer.output.takeRight(2)
      val paint = output(0)
      val shift = output(1)

      panels += (pos -> paint)
      dir = turn(dir, shift)
      pos = advance(pos, dir)
    }
    panels
  }

  def second(programStr: String): String = {
    val program = programStr.split(',').map(_.toLong)
    val panels = paint(program, colors.white)
    val xs = panels.keys.map(p => p._1)
    val ys = panels.keys.map(p => p._2)

    val builder = new StringBuilder()

    for (y <- ys.max to ys.min by -1) {
      builder.append("\n")
      for (x <- xs.min to xs.max) {
        val color = panels.getOrElse((x, y), colors.black)
        builder.append(if (color == colors.white) "█" else " ")
      }
    }
    builder.toString
  }

  private object dirs {
    val up = 0
    val right = 1
    val down = 2
    val left = 3
  }

  private object colors {
    val black = 0
    val white = 1
  }

}
