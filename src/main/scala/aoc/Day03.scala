package aoc

object Day03 {

  def first(wirePaths: Seq[String]): Int = {
    val wires = wirePaths.map(vertices)
    val wire1 = wires(0)
    val wire2 = wires(1)

    val common = wire1.keySet.intersect(wire2.keySet)
    common.map(point => Math.abs(point._1) + Math.abs(point._2)).min
  }

  def second(wirePaths: Seq[String]): Int = {
    val wires = wirePaths.map(vertices)
    val wire1 = wires(0)
    val wire2 = wires(1)

    val common = wire1.keySet.intersect(wire2.keySet)
    common.map(p => wire1(p) + wire2(p)).min
  }

  private def vertices(directions: String): Map[(Int, Int), Int] = {
    var x = 0
    var y = 0
    var delay = 0

    var result: Map[(Int, Int), Int] = Map()

    val dx = Map(('R' -> 1), ('L' -> -1), ('U' -> 0), ('D' -> 0))
    val dy = Map(('R' -> 0), ('L' -> 0), ('U' -> 1), ('D' -> -1))

    for (direction <- directions.split(",")) {
      val dir = direction(0)
      val distance = direction.substring(1).toInt

      for (_ <- 1 to distance) {
        x += dx(dir)
        y += dy(dir)
        delay += 1

        val candidate = (x, y)
        if (!result.contains(candidate))
          result = result + (candidate -> delay)
      }
    }

    result
  }
}
