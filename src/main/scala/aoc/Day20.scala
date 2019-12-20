package aoc

import scala.collection.mutable

object Day20 {

  def first(map: Seq[String]): Int = {
    val (maze, gates) = parse(map)
    val graph = makeGraph(maze, gates)
    bfs(graph, gates.get("AA").head, gates.get("ZZ").head)
  }

  private def bfs(maze: mutable.MultiDict[(Int, Int), (Int, Int)], from: (Int, Int), to: (Int, Int)): Int = {
    val Q = mutable.ArrayDeque[((Int, Int), Int)]()
    val seen = mutable.Set[(Int, Int)]()

    Q.addOne((from, 0))
    seen.add(from)

    while (Q.nonEmpty) {
      val (pos, cost) = Q.removeHead()
      if (pos == to)
        return cost
      for (candidate <- maze.get(pos))
        if (seen.add(candidate))
          Q.addOne((candidate, cost + 1))
    }
    throw new Exception("Failed to find a path")
  }

  private def makeGraph(maze: Set[(Int, Int)], gates: mutable.MultiDict[String, (Int, Int)]): mutable.MultiDict[(Int, Int), (Int, Int)] = {
    val graph = mutable.MultiDict[(Int, Int), (Int, Int)]()

    for (point <- maze) {
      for (candidate <- localTo(point))
        if (maze.contains(candidate))
          graph.addOne((point, candidate))

      for (gate <- gates.keySet) {
        val outs = gates.get(gate)
        if (outs.size == 2 && outs.contains(point))
          graph.addAll(outs.filter(o => o != point).map(o => (point, o)))
      }
    }

    graph
  }

  def second(map: Seq[String]): Int = {
    val (maze, gates) = parse(map)
    val graph = makeRecursiveGraph(maze, gates, map.map(l => l.length).max, map.size)
    bfsRecursive(graph, gates.get("AA").head, gates.get("ZZ").head)
  }

  private def makeRecursiveGraph(maze: Set[(Int, Int)], gates: mutable.MultiDict[String, (Int, Int)], width: Int, height: Int):
  mutable.MultiDict[(Int, Int), ((Int, Int), Int)] = {
    val graph = mutable.MultiDict[(Int, Int), ((Int, Int), Int)]()

    for (point <- maze) {
      for (candidate <- localTo(point))
        if (maze.contains(candidate))
          graph.addOne((point, (candidate, 0)))

      for (gate <- gates.keySet) {
        val outs = gates.get(gate)
        if (outs.size == 2 && outs.contains(point)) {
          val otherEnd = outs.filter(o => o != point).head
          val offset = if (Seq(2, height - 3).contains(point._2) || Seq(2, width - 3).contains(point._1)) -1 else 1
          graph.addOne(((point, (otherEnd, offset))))
        }
      }
    }

    graph
  }

  private def localTo(point: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = point
    Seq((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
  }

  private def parse(map: Seq[String]) = {

    val maze = mutable.Set[(Int, Int)]()
    val gates = mutable.MultiDict[String, (Int, Int)]()
    for (y <- map.indices) {
      for (x <- map(y).indices) {
        val point = map(y)(x)

        if (point == '.') {
          maze.add(x, y)
        } else if (point.isUpper) {
          if (y < map.length - 1 && x < map(y + 1).length && map(y + 1)(x).isUpper) {
            val gatePos = if (y > 0 && map(y - 1)(x) == '.') (x, y - 1) else (x, y + 2)
            val gateName = s"${map(y)(x)}${map(y + 1)(x)}"
            gates.addOne((gateName, gatePos))
          } else if (x < map(y).length - 1 && map(y)(x + 1).isUpper) {
            val gatePos = if (x > 0 && map(y)(x - 1) == '.') (x - 1, y) else (x + 2, y)
            val gateName = s"${map(y)(x)}${map(y)(x + 1)}"
            gates.addOne((gateName, gatePos))
          }
        }
      }
    }
    (maze.toSet, gates)
  }

  private def bfsRecursive(maze: mutable.MultiDict[(Int, Int), ((Int, Int), Int)], from: (Int, Int), to: (Int, Int)): Int = {
    val Q = mutable.ArrayDeque[((Int, Int), Int, Int)]()
    val seen = mutable.Set[((Int, Int), Int)]()

    Q.addOne((from, 0, 0))
    seen.add((from, 0))

    while (Q.nonEmpty) {
      val (pos, cost, level) = Q.removeHead()
      if (pos == to && level == 0)
        return cost
      for ((candidatePos, candidateLevel) <- maze.get(pos)) {
        val newLevel = candidateLevel + level
        val candidate = (candidatePos, newLevel)
        if (newLevel >= 0 && seen.add(candidate))
          Q.addOne((candidatePos, cost + 1, newLevel))
      }
    }
    throw new Exception("Failed to find a path")
  }
}
