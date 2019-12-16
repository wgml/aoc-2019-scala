package aoc

object Day16 {

  def first(input: String, repeat: Int = 100): String = {
    var signal = input.grouped(1).map(_.toInt).toSeq

    for (_ <- 1 to repeat)
      signal = calculate(signal)
    signal.take(8).mkString
  }

  def second(input: String, repeat: Int = 100): String = {
    val signal = input.grouped(1).map(_.toInt).toSeq
    val offset = input.take(7).toInt

    if (offset < signal.size / 2) {
      println("Good luck")
      var relevant = Iterator.continually(signal).flatten.take(10000 * signal.size).toSeq
      for (_ <- 1 to repeat)
        relevant = calculate(relevant)
      relevant.slice(offset, offset + 8).mkString
    } else {
      predict(signal, repeat, signal.length, offset)
    }
  }

  private def predict(signal: Seq[Int], repeat: Int, size: Int, offset: Int): String = {
    val relevant = (offset to size * 10000).map(i => signal(i % size)).toArray
    for (_ <- 1 to repeat) {
      var sum = 0
      for (i <- relevant.length - 1 to 0 by -1) {
        sum += relevant(i)
        relevant(i) = sum.abs % 10
      }
    }
    relevant.take(8).mkString
  }

  private def calculate(signal: Iterable[Int]): Seq[Int] = {
    val count = signal.size

    def calcElement(index: Int): Int =
      (signal zip pattern(index, count)).map(e => e._1 * e._2).sum.abs % 10

    (1 to count).map(i => calcElement(i))
  }

  private def pattern(pos: Int, count: Int): Iterator[Int] = {
    val base = Seq(0, 1, 0, -1)
    val generated = base.flatMap(e => Seq.fill(pos)(e))
    Iterator.continually(generated).flatten.slice(1, count + 1)
  }
}
