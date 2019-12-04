package aoc

import org.scalatest.FunSuite

class Day04Test extends FunSuite {
  test("Day04.stringRep") {
    assert(Day04.strRep(123) === "000123")
    assert(Day04.strRep(123456) === "123456")
  }

  test("Day04.increasing") {
    assert(Day04.increasing("001122") === true)
    assert(Day04.increasing("111111") === true)
    assert(Day04.increasing("123465") === false)
    assert(Day04.increasing("212346") === false)
  }
  test("Day04.duplicates") {
    assert(Day04.duplicates("123252") === false)
    assert(Day04.duplicates("001122") === true)
    assert(Day04.duplicates("111111") === true)
    assert(Day04.duplicates("123466") === true)
    assert(Day04.duplicates("222346") === true)
  }
  test("Day04.duplicatesExact") {
    assert(Day04.duplicatesExact("123252") === false)
    assert(Day04.duplicatesExact("001122") === true)
    assert(Day04.duplicatesExact("111111") === false)
    assert(Day04.duplicatesExact("123666") === false)
    assert(Day04.duplicatesExact("222346") === false)
  }

}
