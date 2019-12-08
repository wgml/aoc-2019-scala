package aoc

object Day08 {
  def first(data: String, width: Int = 25, height: Int = 6): Int = {
    val mostZeros = data.grouped(width * height)
      .minBy(layer => countOf(layer, '0'))
    countOf(mostZeros, '1') * countOf(mostZeros, '2')
  }

  def countOf(layer: String, digit: Char): Int = {
    layer.count(c => c == digit)
  }

  def second(data: String, width: Int = 25, height: Int = 6): String = {
    data.grouped(width * height)
      .reduce(
        (a, b) => (a zip b)
          .map(p => overlap(p._1, p._2))
          .mkString
      )
      .map(c => if (c == '1') "█" else " ")
      .mkString.grouped(width).mkString("\n")
  }

  def overlap(top: Char, bottom: Char): Char = {
    val transparent = '2'
    if (top != transparent) top else bottom
  }
}
