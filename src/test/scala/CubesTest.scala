import aoc.Cubes
import org.scalatest.FunSuite

class CubesTest extends FunSuite {
  test("Cubes.cube") {
    assert(Cubes.cube(3) === 27)
  }
}
