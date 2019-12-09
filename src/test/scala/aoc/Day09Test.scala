package aoc

import aoc.util.Intcode
import org.scalatest.FunSuite

class Day09Test extends FunSuite {
  test("Day09.quine") {
    val program = Array(109L, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)
    val computer = new Intcode(program)
    computer.execute()
    assert(computer.output === program)
  }
  test("Day09.bigInts") {
    {
      val computer = new Intcode(Array(1102L, 34915192, 34915192, 7, 4, 7, 99, 0))
      computer.execute()
      val num = computer.output.head
      assert(num.toString.length === 16)
    }
    {
      val num = 1125899906842624L
      val computer = new Intcode(Array(104L, 1125899906842624L, 99))
      computer.execute()

      assert(computer.output === Array(num))
    }
  }
}
