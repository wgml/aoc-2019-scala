package aoc

object Day12 {

  def first(positions: Seq[String], steps: Int = 1000): Long = {
    val moons = parse(positions)
    val simulated = simulate(moons, steps)
    energy(simulated)
  }

  private def energy(moons: Seq[Moon]): Int = {
    def abs(xyz: XYZ): Int = xyz.x.abs + xyz.y.abs + xyz.z.abs

    moons.map(m => abs(m.pos) * abs(m.vel)).sum
  }

  private def parse(positions: Seq[String]): Seq[Moon] = {
    val pattern = "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>".r
    positions.map(pos => {
      val pattern(x, y, z) = pos
      Moon(XYZ(x.toInt, y.toInt, z.toInt))
    })
  }

  private def simulate(moons: Seq[Moon], steps: Int): Seq[Moon] = {

    def adjust(vel: XYZ, pos: XYZ, other: XYZ): XYZ = {
      def mod(a: Int, b: Int): Int = {
        if (a == b)
          0
        else if (a > b)
          -1
        else 1
      }

      vel.x += mod(pos.x, other.x)
      vel.y += mod(pos.y, other.y)
      vel.z += mod(pos.z, other.z)
      vel
    }

    var state = moons
    for (_ <- 1 to steps) {
      var newState = Seq[Moon]()
      for (moon <- state) {
        val newMoon = moon.copy(moon.pos.copy(), moon.vel.copy())
        for (other <- state)
          newMoon.vel = adjust(newMoon.vel, moon.pos, other.pos)

        newMoon.pos.x += newMoon.vel.x
        newMoon.pos.y += newMoon.vel.y
        newMoon.pos.z += newMoon.vel.z
        newState = newState :+ newMoon
      }
      state = newState
    }
    state
  }

  def second(positions: Seq[String]): Long = {
    val moons = parse(positions)
    (0 to 2).map(a => findCycleOnAxis(moons, a)).reduce((a, b) => util.Math.lcm(a, b))
  }

  private def findCycleOnAxis(moons: Seq[Moon], index: Int): Long = {
    def axis(xyz: XYZ): Int =
      index match {
        case 0 => xyz.x
        case 1 => xyz.y
        case 2 => xyz.z
      }

    def stringify(moons: Seq[Moon]): String = moons.map(m => s"${axis(m.pos)},${axis(m.vel)}").mkString("|")

    var state = moons
    var cache: Map[String, Int] = Map() + (stringify(state) -> 0)

    for (i <- 1 to Int.MaxValue) {
      state = simulate(state, 1)
      val str = stringify(state)
      if (cache.contains(str))
        return i - cache(str)
      else
        cache += (str -> i)
    }
    throw new Exception("Failed to find a cycle")
  }

  private case class XYZ(var x: Int = 0, var y: Int = 0, var z: Int = 0)

  private case class Moon(var pos: XYZ, var vel: XYZ = XYZ())

}
