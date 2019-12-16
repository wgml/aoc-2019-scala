package aoc

import org.scalatest.FunSuite

class Day16Test extends FunSuite {
  test("Day16.first") {
    assert(Day16.first("12345678", 1) === "48226158")
    assert(Day16.first("12345678", 2) === "34040438")
    assert(Day16.first("12345678", 3) === "03415518")
    assert(Day16.first("12345678", 4) === "01029498")
    assert(Day16.first("80871224585914546619083218645595", 100) === "24176176")
    assert(Day16.first("19617804207202209144916044189917", 100) === "73745418")
    assert(Day16.first("69317163492948606335995924319873", 100) === "52432133")
  }

  test("Day16.second") {
    assert(Day16.second("03036732577212944063491565474664", 100) === "84462026")
    assert(Day16.second("02935109699940807407585447034323", 100) === "78725270")
    assert(Day16.second("03081770884921959731165446850517", 100) === "53553731")
  }
}
