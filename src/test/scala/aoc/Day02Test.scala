package aoc
import aoc.util.Intcode
import org.scalatest.FunSuite

class Day02Test extends FunSuite {
  private def execute(program: Array[Int]) : Array[Long] = {
    val computer = new Intcode(program.map(_.toLong))
    computer.execute()
    computer.rawMemory
  }

  test("Day02.execute") {
    assert(execute(Array(1,0,0,0,99)) === Array(2,0,0,0,99))
    assert(execute(Array(2,3,0,3,99)) === Array(2,3,0,6,99))
    assert(execute(Array(2,4,4,5,99,0)) === Array(2,4,4,5,99,9801))
    assert(execute(Array(1,1,1,4,99,5,6,0,99)) === Array(30,1,1,4,2,5,6,0,99))
  }

}
