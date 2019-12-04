package aoc

object Day04 {

  def first(rangeStr: String): Int = {
    val (min, max) = range(rangeStr)
    (min to max).map(strRep).filter(increasing).count(duplicates)
  }

  def duplicates(num: String): Boolean = {
    (num zip num.drop(1)).exists(pair => pair._1 == pair._2)
  }

  def second(rangeStr: String): Int = {
    val (min, max) = range(rangeStr)
    (min to max).map(strRep).filter(increasing).count(duplicatesExact)
  }

  private def range(range: String) = {
    val parts = range.split("-", 2).map(_.toInt).take(2)
    (parts(0), parts(1))
  }

  def strRep(num: Int): String = {
    f"$num%06d"
  }

  def increasing(num: String): Boolean = {
    (num zip num.drop(1)).forall(pair => pair._1 <= pair._2)
  }

  def duplicatesExact(num: String): Boolean = {
    def checkOne(i: Int): Boolean = {
      if (num(i - 1) != num(i))
        return false
      if (i > 1 && num(i - 2) == num(i))
        return false

      i == num.length - 1 || num(i + 1) != num(i)
    }

    (1 until num.length).exists(checkOne)
  }

}
