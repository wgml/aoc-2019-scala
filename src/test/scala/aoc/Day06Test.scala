package aoc

import org.scalatest.FunSuite

class Day06Test extends FunSuite {
  test("Day06.first") {
    val map="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L".split('\n')
    assert(Day06.first(map) === 42)
  }
  test("Day06.second") {
    val map="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".split('\n')
    assert(Day06.second(map) === 4)
  }

}
