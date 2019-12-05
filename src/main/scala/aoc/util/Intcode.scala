package aoc.util

class Intcode {
  var output: Array[Int] = Array()

  def execute(program: Array[Int], input: Iterator[Int] = Iterator()): Array[Int] = {
    var pc = 0

    while (pc < program.length) {
      val instr = program(pc)
      val opcode = instr % 100

      if (opcode == 1) {
        val a = read(program, pc, 1, instr)
        val b = read(program, pc, 2, instr)
        val resultCell = program(pc + 3)

        program(resultCell) = a + b

        pc += 4
      }
      else if (opcode == 2) {
        val a = read(program, pc, 1, instr)
        val b = read(program, pc, 2, instr)
        val resultCell = program(pc + 3)

        program(resultCell) = a * b

        pc += 4
      } else if (opcode == 3) {
        val resultCell = program(pc + 1)
        program(resultCell) = input.next()

        pc += 2
      } else if (opcode == 4) {
        output = output :+ read(program, pc, 1, instr)

        pc += 2
      } else if (opcode == 5) {
        val cond = read(program, pc, 1, instr)
        val address = read(program, pc, 2, instr)
        if (cond != 0) {
          pc = address
        } else {
          pc += 3
        }
      } else if (opcode == 6) {
        val cond = read(program, pc, 1, instr)
        val address = read(program, pc, 2, instr)
        if (cond == 0) {
          pc = address
        } else {
          pc += 3
        }
      } else if (opcode == 7) {
        val a = read(program, pc, 1, instr)
        val b = read(program, pc, 2, instr)
        val resultCell = program(pc + 3)

        program(resultCell) = if ((a < b)) 1 else 0

        pc += 4
      } else if (opcode == 8) {
        val a = read(program, pc, 1, instr)
        val b = read(program, pc, 2, instr)
        val resultCell = program(pc + 3)

        program(resultCell) = if ((a == b)) 1 else 0

        pc += 4
      } else {
        return program
      }
    }
    program
  }

  def read(program: Array[Int], pc: Int, offset: Int, instr: Int): Int = {
    val mode = parameter(instr, offset)
    val value = program(pc + offset)
    if (mode == 0) {
      return program(value)
    }
    value
  }

  def parameter(instr: Int, index: Int): Int = {
    (instr / math.pow(10, index + 2 - 1).toInt % 10)
  }
}
