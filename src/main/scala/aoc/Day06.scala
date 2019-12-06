package aoc

import scala.collection.mutable

object Day06 {
  def first(orbitsStr: Seq[String]): Int = {
    val orbits = generate(orbitsStr)
    countOrbits(orbits)
  }

  private def generate(orbits: Seq[String]): mutable.MultiDict[String, String] = {
    val result: mutable.MultiDict[String, String] = mutable.MultiDict()
    for (orbit <- orbits) {
      val parts = orbit.split(')')
      val src = parts(0)
      val orbiter = parts(1)
      result += (src -> orbiter)
    }
    result
  }

  private def countOrbits(orbits: mutable.MultiDict[String, String]): Int = {
    def countOrbitsFor(planet: String, inc: Int): Int = {
      var res = inc
      for (child <- orbits.get(planet))
        res += countOrbitsFor(child, inc + 1)
      res
    }

    countOrbitsFor("COM", 0)
  }

  def second(orbitsStr: Seq[String]): Int = {
    val orbits = generate(orbitsStr)
    val src = findPlanetFor(orbits, "YOU").get
    val dest = findPlanetFor(orbits, "SAN").get

    val myPath = pathToCom(orbits, src)
    val santaPath = pathToCom(orbits, dest)
    val root = findRoot(myPath, santaPath).get
    myPath.indexOf(root) + santaPath.indexOf(root)
  }

  private def findPlanetFor(orbits: mutable.MultiDict[String, String], orbiter: String): Option[String] = {
    orbits.find((entry) => entry._2.contains(orbiter)).map(_._1)
  }

  private def pathToCom(orbits: mutable.MultiDict[String, String], src: String): Seq[String] = {
    val dest = "COM"
    if (src == dest)
      return Seq(src)
    val next = findPlanetFor(orbits, src).get
    Seq(src) ++ pathToCom(orbits, next)
  }

  private def findRoot(first: Seq[String], second: Seq[String]): Option[String] = {
    first.find(o => second.contains(o))
  }
}
