package aoc

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

  def first(program: String, noun: Int = 12, verb: Int = 2): Int = {
    val cells = program.split(',').map(_.toInt)
    cells(1) = noun
    cells(2) = verb
    val result = execute(cells)
    result(0)
  }

  def execute(cells: Array[Int]): Array[Int] = {
    var pc = 0

    while (pc < cells.length) {
      val instr = cells(pc)

      if (instr == 99)
        return cells

      val a = cells(cells(pc + 1))
      val b = cells(cells(pc + 2))
      val resultCell = cells(pc + 3)

      val result = if (instr == 1) a + b else a * b
      cells(resultCell) = result
      pc += 4
    }

    cells
  }
}
