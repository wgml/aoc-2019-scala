package aoc

import aoc.util.Inputs

object Main extends App {
  val input = Inputs.ints("01")
  println(s"Day01 first=${Day01.first(input)}, second=${Day01.second(input)}")
}
