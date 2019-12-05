package aoc.util

class Intcode(var memory: Array[Int]) {
  var output: Array[Int] = Array()
  private var pc: Int = 0

  def execute(input: Iterator[Int] = Iterator()): Unit = {
    pc = 0

    while (pc < memory.length) {
      val instr = memory(pc)

      instr % 100 match {
        case 1 => add()
        case 2 => multiply()
        case 3 => inputValue(input)
        case 4 => outputValue()
        case 5 => jumpNotZero()
        case 6 => jumpZero()
        case 7 => lessThan()
        case 8 => equalTo()
        case _ => return
      }
    }
  }

  def value(offset: Int): Int = {
    val mode = parameter(offset)
    val value = memory(pc + offset)
    if (mode == 0) {
      return memory(value)
    }
    value
  }

  private def parameter(index: Int): Int =
    (instr / math.pow(10, index + 2 - 1).toInt % 10)

  private def instr = memory(pc)

  private def equalTo(): Unit = {
    val a = value(1)
    val b = value(2)
    val resultCell = memory(pc + 3)

    memory(resultCell) = if ((a == b)) 1 else 0

    pc += 4
  }

  private def lessThan(): Unit = {
    val a = value(1)
    val b = value(2)
    val resultCell = memory(pc + 3)

    memory(resultCell) = if ((a < b)) 1 else 0

    pc += 4
  }

  private def jumpZero(): Unit = {
    val cond = value(1)
    val address = value(2)
    if (cond == 0) {
      pc = address
    } else {
      pc += 3
    }
  }

  private def jumpNotZero(): Unit = {
    val cond = value(1)
    val address = value(2)
    if (cond != 0) {
      pc = address
    } else {
      pc += 3
    }
  }

  private def outputValue(): Unit = {
    output = output :+ value(1)

    pc += 2
  }

  private def inputValue(input: Iterator[Int]): Unit = {
    val resultCell = memory(pc + 1)
    memory(resultCell) = input.next()

    pc += 2
  }

  private def multiply(): Unit = {
    val a = value(1)
    val b = value(2)
    val resultCell = memory(pc + 3)

    memory(resultCell) = a * b

    pc += 4
  }

  private def add(): Unit = {
    val a = value(1)
    val b = value(2)
    val resultCell = memory(pc + 3)

    memory(resultCell) = a + b

    pc += 4
  }
}
