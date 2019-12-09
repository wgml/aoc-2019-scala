package aoc.util

class NoInput extends Exception {}

class Intcode(var rawMemory: Array[Long]) {
  var output: Array[Long] = Array()
  var halted: Boolean = false
  private var pc: Int = 0
  private var base: Int = 0

  def execute(input: Iterator[Long] = Iterator(), reset: Boolean = true): Unit = {
    if (reset) {
      pc = 0
      base = 0
      halted = false
      output = Array()
    }
    else if (!reset && halted)
      throw new Exception

    while (pc < rawMemory.length) {
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
        case 9 => adjustBase()
        case _ =>
          halted = true
          return
      }
    }
  }

  private def memory(address: Int): Long = {
    if (address >= rawMemory.length)
      resizeMemory(address)

    rawMemory(address)
  }

  private def write(offset: Int, value: Long): Unit = {
    val mode = parameter(offset)
    val address = mode match {
      case 0 => memory(pc + offset)
      case 2 => memory(pc + offset) + base
    }

    if (address >= rawMemory.length)
      resizeMemory(address.toInt)

    rawMemory(address.toInt) = value
  }

  private def read(offset: Int): Long = {
    val mode = parameter(offset)
    mode match {
      case 0 => memory(memory(pc + offset).toInt)
      case 1 => memory(pc + offset)
      case 2 => memory(memory(pc + offset).toInt + base)
    }
  }

  private def resizeMemory(address: Int): Unit = {
    rawMemory = (0 to address).map(a => if (a < rawMemory.length) rawMemory(a) else 0).toArray
  }

  private def equalTo(): Unit = {
    val a = read(1)
    val b = read(2)

    write(3, if ((a == b)) 1 else 0)

    pc += 4
  }

  private def lessThan(): Unit = {
    val a = read(1)
    val b = read(2)

    write(3, if ((a < b)) 1 else 0)

    pc += 4
  }

  private def jumpZero(): Unit = {
    val cond = read(1)
    val address = read(2)
    if (cond == 0) {
      pc = address.toInt
    } else {
      pc += 3
    }
  }

  private def jumpNotZero(): Unit = {
    val cond = read(1)
    val address = read(2)
    if (cond != 0) {
      pc = address.toInt
    } else {
      pc += 3
    }
  }

  private def outputValue(): Unit = {
    output = output :+ read(1)

    pc += 2
  }

  private def parameter(index: Long): Long =
    instr / math.pow(10, index + 2 - 1).toLong % 10

  private def instr = memory(pc)

  private def adjustBase(): Unit = {
    base += read(1).toInt

    pc += 2
  }

  private def inputValue(input: Iterator[Long]): Unit = {

    if (!input.hasNext)
      throw new NoInput
    write(1, input.next())

    pc += 2
  }

  private def multiply(): Unit = {
    val a = read(1)
    val b = read(2)

    write(3, a * b)

    pc += 4
  }

  private def add(): Unit = {
    val a = read(1)
    val b = read(2)

    write(3, a + b)

    pc += 4
  }
}
