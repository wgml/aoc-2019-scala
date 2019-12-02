package aoc.util

import scala.io.Source

object Inputs {
  def string(day: "02"): String = {
    val source = Source.fromResource(filename(day))
    val res = source.getLines.next()
    source.close
    res
  }

  def filename(day: String): String = {
    s"input/${day}.txt"
  }

  def ints(day: String): Seq[Int] = {
    val source = Source.fromResource(filename(day))
    val res = source.getLines.map(_.toInt).toSeq
    source.close
    res
  }
}
