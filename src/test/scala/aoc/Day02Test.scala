package aoc

import org.scalatest.FunSuite

class Day02Test extends FunSuite {
  test("Day02.execute") {
    assert(Day02.execute(Array(1,0,0,0,99)) === Array(2,0,0,0,99))
    assert(Day02.execute(Array(2,3,0,3,99)) === Array(2,3,0,6,99))
    assert(Day02.execute(Array(2,4,4,5,99,0)) === Array(2,4,4,5,99,9801))
    assert(Day02.execute(Array(1,1,1,4,99,5,6,0,99)) === Array(30,1,1,4,2,5,6,0,99))
  }

}
