package aoc

import aoc.util.Intcode
import org.scalatest.FunSuite

class Day05Test extends FunSuite {
  test("Day05.immediateMode") {
    val computer = new Intcode(Array(1002, 4, 3, 4, 33))
    computer.execute()
    assert(computer.memory === Array(1002, 4, 3, 4, 99))
  }

  test("Day05.equals") {
    {
      val computer = new Intcode(Array(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8))
      val input = Seq(8)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8))
      val input = Seq(9)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
    {
      val computer = new Intcode(Array(3, 3, 1108, -1, 8, 3, 4, 3, 99))
      val input = Seq(8)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 3, 1108, -1, 8, 3, 4, 3, 99))
      val input = Seq(9)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
  }

  test("Day05.lessThan") {
    {
      val computer = new Intcode(Array(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8))
      val input = Seq(7)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8))
      val input = Seq(9)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
    {
      val computer = new Intcode(Array(3, 3, 1107, -1, 8, 3, 4, 3, 99))
      val input = Seq(7)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 3, 1107, -1, 8, 3, 4, 3, 99))
      val input = Seq(9)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
  }

  test("Day05.jumps") {
    {
      val computer = new Intcode(Array(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9))
      val input = Seq(7)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9))
      val input = Seq(0)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
    {
      val computer = new Intcode(Array(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1))
      val input = Seq(7)
      computer.execute(input.iterator)

      assert(computer.output === Array(1))
    }
    {
      val computer = new Intcode(Array(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1))
      val input = Seq(0)
      computer.execute(input.iterator)

      assert(computer.output === Array(0))
    }
  }

  test("Day05.compares") {
    val program = Array(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

    val computer = new Intcode(program)
    val input = Seq(7, 8, 9)
    val inputIterator = input.iterator
    computer.execute(inputIterator)
    computer.execute(inputIterator)
    computer.execute(inputIterator)

    assert(computer.output === Array(999, 1000, 1001))
  }
}
