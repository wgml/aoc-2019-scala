package aoc

import aoc.util.Inputs

object Main extends App {
  {
    val input = Inputs.ints("01")
    println("Day01")
    println(s"first=${Day01.first(input)}")
    println(s"second=${Day01.second(input)}")
  }
  {
    val input = Inputs.string("02")
    println("Day02")
    println(s"first=${Day02.first(input)}")
    println(s"second=${Day02.second(input)}")
  }
  {
    val input = Inputs.strings("03")
    println("Day03")
    println(s"first=${Day03.first(input)}")
    println(s"second=${Day03.second(input)}")
  }

}
