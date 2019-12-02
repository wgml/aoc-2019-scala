package aoc

object Day01 {
  def first(modules: Seq[Int]): Int = {
    modules.map(m => fuel(m)).sum
  }

  def second(modules: Seq[Int]): Int = {
    modules.map(m => fuelCompensated(m)).sum
  }

  def fuelCompensated(x: Int): Int = {
    var total = 0
    var compensate = fuel(x)

    while (compensate > 0) {
      total += compensate
      compensate = fuel(compensate)
    }

    total
  }

  def fuel(x: Int): Int = {
    (x / 3) - 2
  }
}
