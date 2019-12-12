package aoc

import org.scalatest.FunSuite

class Day12Test extends FunSuite {
  test("Day12.firstAfter10") {
    val input = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>".split('\n')
    assert(Day12.first(input, 10) === 179)
  }
  test("Day12.firstAfter100") {
    val input = "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>".split('\n')
    assert(Day12.first(input, 100) === 1940)
  }

  test("Day12.second") {
    val input = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>".split('\n')
    assert(Day12.second(input) === 2772)
  }

  test("Day12.biggerUniverse") {
    val input = "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>".split('\n')
    assert(Day12.second(input) === 4686774924L)
  }
}
