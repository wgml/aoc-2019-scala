package aoc

object Day14 {

  def first(reactionDefs: Seq[String]): Long = {
    val reactions = parseReactions(reactionDefs)
    val ordered = sortByPrecedence(materials.desired, reactions)

    oreRequiredForFuel(reactions, ordered, 1)
  }

  def second(reactionDefs: Seq[String]): Long = {
    val reactions = parseReactions(reactionDefs)
    val ordered = sortByPrecedence(materials.desired, reactions)

    val oreLimit = 1000000000000L
    var fuelMin = 0L
    var fuelMax = oreLimit

    while (fuelMin != fuelMax) {
      val mid = fuelMin + (fuelMax - fuelMin) / 2
      if (oreRequiredForFuel(reactions, ordered, mid) <= oreLimit)
        fuelMin = mid + 1
      else
        fuelMax = mid
    }

    fuelMin - 1
  }

  private def oreRequiredForFuel(reactions: Map[String, Reaction], ordered: Seq[String], fuel: Long) = {
    def calcRepetitions(num: Long, den: Long): Long = (num + den - 1) / den

    var required = Map[String, Long]()
    required += (materials.desired -> fuel)
    for (material <- ordered) {
      val reaction = reactions(material)
      for ((ingredient, capacity) <- reaction.in) {
        if (!required.contains(ingredient))
          required += (ingredient -> 0)

        val repetitions = calcRepetitions(required(material), reaction.out(material))
        required += (ingredient -> (required(ingredient) + repetitions * capacity))
      }
    }
    required(materials.resource)
  }

  def sortByPrecedence(root: String, reactions: Map[String, Reaction]): Seq[String] = {
    var visited: Set[String] = Set()
    var result: Seq[String] = Seq()

    def doOrder(material: String): Unit = {
      if (material == materials.resource)
        return
      visited += material
      val reaction = reactions(material)
      val reliesOn = reaction.in.keys
      for (m <- reliesOn)
        if (!visited.contains(m))
          doOrder(m)
      result = Seq(material) ++ result
    }

    doOrder(root)
    result
  }

  def parseReactions(reactionDefs: Seq[String]): Map[String, Reaction] = {
    def ingredients(str: String): Map[String, Int] = {
      str.split(", ").map(ing => {
        val parts = ing.split(" ")
        parts(1) -> parts(0).toInt
      }).toMap
    }

    val reactions = reactionDefs.map(d => d.split(" => "))
      .map(parts => Reaction(ingredients(parts(0)), ingredients(parts(1))))

    reactions.flatMap(r => r.in.keys ++ r.out.keys)
      .distinct
      .filter(m => m != materials.resource)
      .map(m => m -> reactions.find(r => r.out.contains(m)).get)
      .toMap
  }

  case class Reaction(in: Map[String, Int], out: Map[String, Int])

  private object materials {
    val resource = "ORE"
    val desired = "FUEL"
  }

}
