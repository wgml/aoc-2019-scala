package aoc

import org.scalatest.FunSuite

class Day08Test extends FunSuite {
  test("Day08.first") {
    assert(Day08.first("123456789012", 3, 2) === 1)
  }

  test("Day08.second") {
    assert(Day08.second("0222112222120000", 2, 2) === " █\n█ ")
  }

}
