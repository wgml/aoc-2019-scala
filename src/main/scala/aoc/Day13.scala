package aoc

import aoc.util.Intcode

object Day13 {

  def first(program: String): Long = {
    val output = execute(program)
    blocks(read(output))
  }

  private def execute(programStr: String) = {
    val program = programStr.split(',').map(_.toLong)
    val computer = new Intcode(program)
    computer.execute(Seq().iterator)

    computer.output
  }

  def second(program: String): Long = {
    play(program)
  }

  private def play(programStr: String): Int = {
    def horizontalPos(map: Map[Pos, Int], value: Int): Int =
      map.find(e => e._2 == value && (e._1.x != -1 || e._1.y != 0)).get._1.x

    def score(map: Map[Pos, Int]) = map.find(e => e._1.x == -1 && e._1.y == 0).get._2

    def AI(paddle: Int, ball: Int): Int = math.signum(ball - paddle)

    val program = programStr.split(',').map(_.toLong)
    val computer = new Intcode(program)
    computer.withMemoryOverride(0, 2)

    var nextInput = 1L
    while (!computer.halted) {
      computer.executeAndWaitForInput(Seq(nextInput).iterator)
      val screen = read(computer.output)
      val paddle = horizontalPos(screen, objects.paddle)
      val ball = horizontalPos(screen, objects.ball)

      if (blocks(screen) == 0)
        return score(screen)

      nextInput = AI(paddle, ball)
    }
    throw new Exception("Unreachable code")
  }

  def blocks(map: Map[Pos, Int]): Int = map.values.count(v => v == objects.block)

  def read(output: Array[Long]): Map[Pos, Int] =
    output.map(_.toInt).grouped(3).map(thrice => Pos(thrice(0), thrice(1)) -> thrice(2)).toMap

  case class Pos(x: Int, y: Int)

  private object objects {
    val empty = 0
    val wall = 1
    val block = 2
    val paddle = 3
    val ball = 4
  }

}
