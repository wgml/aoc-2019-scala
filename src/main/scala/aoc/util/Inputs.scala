package aoc.util

import scala.io.Source

object Inputs {
  def filename(day: String) = {
    s"input/${day}.txt"
  }

  def ints(day: String): Seq[Int] = {
    val source = Source.fromResource(filename(day))
    val res = source.getLines.map(_.toInt).toSeq
    source.close
    res
  }
}
