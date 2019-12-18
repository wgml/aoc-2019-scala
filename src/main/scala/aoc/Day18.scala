package aoc

import scala.collection.mutable

object Day18 {

  private val dirs = Seq((0, 1), (0, -1), (1, 0), (-1, 0))

  def second(originalMap: Seq[String]): Int = {
    val (x, y) = findEntrances(originalMap).head
    val map = updateMap(originalMap, x, y)

    val entrances = findEntrances(map)
    val keys = find(map, types.key)

    var Q = mutable.ArrayDeque[MultiState]()
    Q += MultiState(entrances.toSeq, Set[Char](), 0)

    var seen = Map[String, Int]()

    var best = Integer.MAX_VALUE

    def isTraversable(state: MultiState): Boolean = {
      for ((x, y) <- state.pos) {
        if (y < 0 || y >= map.length || x < 0 || x >= map(y).length || map(y)(x) == types.wall)
          return false
        if (types.door(map(y)(x)) && !state.keys.contains(map(y)(x).toLower))
          return false
      }
      true
    }

    while (Q.nonEmpty) {
      val state = Q.removeHead()
      val keyStr = state.pos.mkString("|") + state.keys.toSeq.sorted.toString
      if ((!seen.contains(keyStr)) || state.dist < seen(keyStr)) {
        seen += (keyStr -> state.dist)

        if (isTraversable(state)) {
          val traversed = traverseWithoutKeys(map, state)
          val (candidates, localBest) = resolveKeyDependencies(state, traversed, keys)
          Q.addAll(candidates)
          best = math.min(best, localBest)
        }
      }
    }
    best
  }

  private def resolveKeyDependencies(state: MultiState, traversed: Map[(Int, Int), (Int, Int)], keys: Map[Char, (Int, Int)]): (Seq[MultiState], Int) = {
    var best = Integer.MAX_VALUE
    var candidates = Seq[MultiState]()
    for ((key, p) <- keys) {
      if (!state.keys.contains(key) && traversed.contains(p)) {
        val (distance, robot) = traversed(p)
        val newPos = state.pos.toArray
        newPos(robot) = p
        val newKeys = mutable.Set[Char]() ++ state.keys
        newKeys.add(key)
        val newDist = state.dist + distance

        if (newKeys.size == keys.size)
          if (best > newDist)
            best = newDist

        candidates = candidates :+ MultiState(newPos, newKeys.toSet, newDist)
      }
    }
    (candidates, best)
  }

  private def traverseWithoutKeys(map: Seq[String], state: MultiState): Map[(Int, Int), (Int, Int)] = {
    val frontier = mutable.ArrayDeque[((Int, Int), Int, Int)]()
    var traversed = Map[(Int, Int), (Int, Int)]()
    for (i <- state.pos.indices)
      frontier.addOne((state.pos(i), i, 0))

    while (frontier.nonEmpty) {
      val ((x, y), robot, dist) = frontier.removeHead()

      if (!wallOrOutOfBounds(map, x, y)) {
        val point = map(y)(x)
        if (!types.door(point) || state.keys.contains(point.toLower)) {
          if (!traversed.contains((x, y))) {
            traversed += ((x, y) -> (dist, robot))
            for ((dx, dy) <- dirs)
              frontier.addOne(((x + dx, y + dy), robot, dist + 1))
          }
        }
      }
    }
    traversed
  }

  def updateMap(map: Seq[String], px: Int, py: Int): Seq[String] = {
    var result = ""

    for (y <- map.indices) {
      val dy = (py - y).abs
      for (x <- map(y).indices) {
        val dx = (px - x).abs

        if ((dx == 0 && dy == 1) || (dx == 1 && dy == 0))
          result += types.wall
        else if (dx == 0 && dy == 0)
          result += types.wall
        else if (dx == 1 && dy == 1)
          result += types.entrance
        else
          result += map(y)(x)
      }
      result += "\n"
    }
    result.split('\n')
  }

  private def findEntrances(map: Seq[String]): Set[(Int, Int)] = {
    val entrances = mutable.Set[(Int, Int)]()
    for (y <- map.indices)
      for (x <- map(y).indices)
        if (map(y)(x) == types.entrance)
          entrances.add((x, y))
    entrances.toSet
  }

  def first(map: Seq[String]): Int = {

    val (x, y) = find(map, p => p == types.entrance).head._2
    val keys = find(map, types.key)

    var frontier = mutable.ArrayDeque[State]()
    frontier += State(x, y, Set[Char](), 0)

    val seen = mutable.Set[((Int, Int), String)]()

    def checkCandidate(state: State): (Seq[State], Option[Int]) = {
      val pos = (state.x, state.y)
      val keyStr = state.keys.toSeq.sorted.toString
      if (seen.contains((pos, keyStr)))
        return (Seq(), None)

      seen += ((pos, keyStr))

      if (wallOrOutOfBounds(map, state.x, state.y))
        return (Seq(), None)

      val point = map(state.y)(state.x)

      if (types.door(point) && !state.keys.contains(point.toLower))
        return (Seq(), None)

      val newKeys = mutable.Set[Char]() ++ state.keys
      if (types.key(point)) {
        newKeys += point
        if (newKeys.size == keys.size)
          return (Seq(), Some(state.dist))
      }
      (dirs.map(d => State(state.x + d._1, state.y + d._2, newKeys.toSet, state.dist + 1)), None)
    }

    while (frontier.nonEmpty) {
      val state = frontier.removeHead()
      val (candidates, solution) = checkCandidate(state)
      solution match {
        case Some(v) => return v
        case None => frontier.addAll(candidates)
      }
    }
    throw new Exception("Did not find the solution")
  }

  private def wallOrOutOfBounds(map: Seq[String], x: Int, y: Int): Boolean =
    y < 0 || y >= map.length || x < 0 || x >= map(y).length || map(y)(x) == types.wall

  private def find(map: Seq[String], pred: (Char) => Boolean): Map[Char, (Int, Int)] = {
    var result = Map[Char, (Int, Int)]()
    for (y <- map.indices)
      for (x <- map(y).indices)
        if (pred(map(y)(x)))
          result += (map(y)(x) -> (x, y))
    result
  }

  private case class State(x: Int, y: Int, keys: Set[Char], dist: Int)

  private case class MultiState(pos: Seq[(Int, Int)], keys: Set[Char], dist: Int)

  private object types {
    val entrance = '@'
    val passage = '.'
    val wall = '#'

    def door(c: Char): Boolean = c.isLetter && c.isUpper

    def key(c: Char): Boolean = c.isLetter && c.isLower
  }

}
