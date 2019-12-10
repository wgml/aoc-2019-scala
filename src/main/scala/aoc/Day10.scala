package aoc

object Day10 {

  implicit class Crossable[X](xs: Iterable[X]) {
    def cross[Y](ys: Iterable[Y]): Iterable[(X, Y)] = for {x <- xs; y <- ys} yield (x, y)
  }

  def first(mapStr: Seq[String]): Int = {
    val map = parseMap(mapStr);
    val (_, count) = findStationCoordinates(map)
    count
  }

  def parseMap(mapStr: Seq[String]): Array[Array[Boolean]] = {
    mapStr.map(line => line.map(c => c == '#').toArray).toArray
  }

  private def findStationCoordinates(map: Array[Array[Boolean]]): ((Int, Int), Int) = {
    (map(0).indices cross map.indices)
      .filter(p => map(p._2)(p._1))
      .map(p => (p, calculateReachable(map, p._1, p._2).size))
      .maxBy(p => p._2)
  }

  private def calculateReachable(map: Array[Array[Boolean]], x: Int, y: Int): Set[(Int, Int)] = {
    @scala.annotation.tailrec
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    (map(0).indices cross map.indices)
      .filter(p => map(p._2)(p._1) && (p._1 != x || p._2 != y))
      .map(p => {
        val dx = p._1 - x
        val dy = p._2 - y
        val divisor = gcd(dx.abs, dy.abs)
        (dx / divisor, dy / divisor)
      })
      .toSet
  }

  def second(mapStr: Seq[String]): Int = {
    val map = parseMap(mapStr);

    val (station, _) = findStationCoordinates(map)
    val destroyed = findNthDestroyed(map, station, 200)

    100 * destroyed._1 + destroyed._2
  }

  def findNthDestroyed(map: Array[Array[Boolean]], center: (Int, Int), count: Int): (Int, Int) = {
    def findInDirection(offset: (Int, Int)): (Int, Int) = {
      for (increment <- 1 to math.max(map.length, map(0).length)) {
        val c = (center._1 + increment * offset._1, center._2 + increment * offset._2)
        if (c._1 >= 0 && c._2 >= 0 && map.length >= c._2 && map(c._2).length >= c._1 && map(c._2)(c._1))
          return c
      }
      throw new Exception("Did not find expected point on the map")
    }

    def clockwise(point: (Int, Int)) = if (point._1 == 0) -10000000.0 else 1.0 * point._2 / point._1

    def byYAxis(point: (Int, Int)) = point._1 > 0 || (point._1 == 0 && point._2 <= 0)

    var remaining = count
    while (remaining > 0) {
      assert(map.map(line => line.count(c => c)).sum > remaining)
      val reachable = calculateReachable(map, center._1, center._2)
        .groupBy(byYAxis)
        .map(half => {
          val points = half._2.toSeq.sortBy(clockwise)
          (half._1, points)
        })
        .toSeq
        .sortBy(half => !half._1)
        .flatMap(half => half._2)
        .map(findInDirection)

      if (reachable.length >= remaining) {
        val nth = reachable(remaining - 1)
        return nth
      } else {
        remaining -= reachable.length
        for (e <- reachable)
          map(e._2)(e._1) = false
      }
    }

    throw new Exception("Unreachable point")
  }
}
