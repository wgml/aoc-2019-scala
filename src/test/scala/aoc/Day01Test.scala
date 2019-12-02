package aoc

import org.scalatest.FunSuite

class Day01Test extends FunSuite {
  test("Day01.fuel") {
    assert(Day01.fuel(12) === 2)
    assert(Day01.fuel(14) === 2)
    assert(Day01.fuel(1969) === 654)
    assert(Day01.fuel(100756) === 33583)
  }

  test("Day01.fuelCompensated") {
    assert(Day01.fuelCompensated(14) === 2)
    assert(Day01.fuelCompensated(1969) === 966)
    assert(Day01.fuelCompensated(100756) === 50346)
  }
}
