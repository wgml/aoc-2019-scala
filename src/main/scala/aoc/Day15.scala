package aoc

import aoc.util.{Intcode, NoInput}

import scala.collection.mutable

object Day15 {

  def first(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    bfs(program)._1.size
  }

  def second(programStr: String): Long = {
    val program = programStr.split(',').map(_.toLong)
    val tankPath = bfs(program)._1
    bfs(program, tankPath)._2
  }

  private def bfs(program: Array[Long], pathPrefix: Seq[Long] = Seq()): (Seq[Long], Int) = {
    val visited = mutable.Set[(Int, Int)]()

    def markVisited(path: Seq[Long]): Boolean = {
      var (x, y) = (0, 0)
      for (d <- path) {
        d match {
          case dir.north => y += 1
          case dir.south => y -= 1
          case dir.west => x -= 1
          case dir.east => x += 1
        }
      }
      visited.add((x, y))
    }

    def executeForPath(path: Seq[Long]): Long = {
      val computer = new Intcode(program.clone)
      try {
        computer.execute(path.iterator)
      } catch {
        case _: NoInput => ;
      }
      computer.output.last
    }

    val queue = new mutable.Queue[Seq[Long]]()
    queue.addAll(dirs().map(d => pathPrefix ++ Seq(d)))

    var oxygenTankPath: Seq[Long] = Seq()
    var currentPath: Seq[Long] = Seq()

    while (queue.nonEmpty) {
      currentPath = queue.removeHead()
      if (markVisited(currentPath)) {
        executeForPath(currentPath) match {
          case reply.wall =>
          case reply.moved =>
            queue.addAll(dirs().map(d => currentPath ++ Seq(d)))
          case reply.tank => oxygenTankPath = currentPath
        }
      }
    }

    (oxygenTankPath, currentPath.size - pathPrefix.size - 1)
  }

  private def dirs(): Seq[Long] = Seq(dir.north, dir.south, dir.west, dir.east)

  private object reply {
    val wall = 0
    val moved = 1
    val tank = 2
  }

  private object dir {
    val north = 1L
    val south = 2L
    val west = 3L
    val east = 4L
  }

}
