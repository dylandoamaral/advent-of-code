import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17Part2 {

  case class Program (registerA: Long, registerB: Long, registerC: Long, outputs: List[Int], instructionPointer: Int,  instructions: Array[Int]) {
    def run(): List[Int] =
      if (isFinished) outputs
      else runOneInstruction().run()

    def runOneInstruction(): Program = {
      val instruction = instructions(instructionPointer)
      val literalOperand = instructions(instructionPointer + 1)
      def comboOperand = literalOperand match
        case 0 => 0L
        case 1 => 1L
        case 2 => 2L
        case 3 => 3L
        case 4 => registerA
        case 5 => registerB
        case 6 => registerC
        case 7 => throw new Exception("Combo operand 7 is reserved and will not appear in valid programs.")

      instruction match
        case 0 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerA = (numerator / denominator).toLong, instructionPointer = instructionPointer + 2)
        case 1 =>
          this.copy(registerB = registerB ^ literalOperand, instructionPointer = instructionPointer + 2)
        case 2 =>
          this.copy(registerB = comboOperand % 8, instructionPointer = instructionPointer + 2)
        case 3 =>
          if (registerA == 0) this.copy(instructionPointer = instructionPointer + 2)
          else this.copy(instructionPointer = literalOperand)
        case 4 =>
          this.copy(registerB = registerB ^ registerC, instructionPointer = instructionPointer + 2)
        case 5 =>
          this.copy(outputs = outputs :+ (comboOperand % 8).toInt, instructionPointer = instructionPointer + 2)
        case 6 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerB = (numerator / denominator).toLong, instructionPointer = instructionPointer + 2)
        case 7 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerC = (numerator / denominator).toLong, instructionPointer = instructionPointer + 2)
    }

    def isFinished: Boolean = instructionPointer >= instructions.length
  }

  object Program {
    def parse(lines: Array[String]): Program = Program(
      registerA = lines(0).dropWhile(!_.isDigit).toLong,
      registerB = lines(1).dropWhile(!_.isDigit).toLong,
      registerC = lines(2).dropWhile(!_.isDigit).toLong,
      outputs = List.empty,
      instructionPointer = 0,
      instructions = lines(4).dropWhile(!_.isDigit).split(",").map(_.toInt),
    )
  }

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day17.txt"))(_.getLines().toArray).get
    val program = Program.parse(input)
    val expectedOutputs = program.instructions.toList
    val answer = simulate(program, expectedOutputs, 0)

    println(answer)
  }

  @tailrec
  def simulate(program: Program, expectedOutputs: List[Int], current: Long): Long = {
    val outputs = program.copy(registerA = current).run()

    if (outputs == expectedOutputs) current
    else if (outputs.length <= expectedOutputs.length - 3) simulate(program = program, expectedOutputs = expectedOutputs, current = current + 1000000000L)
    else if (outputs.length != expectedOutputs.length) simulate(program = program, expectedOutputs = expectedOutputs, current = current + 100000000L)
    else {
      val reverseOutputs = outputs.reverse
      val reverseExpectedOutputs = expectedOutputs.reverse
      val sameDigits = reverseOutputs.zip(reverseExpectedOutputs).takeWhile(_ == _).length

      if (sameDigits < 7) simulate(program = program, expectedOutputs = expectedOutputs, current = current + 100000000L / Math.pow(10, sameDigits + 1).toInt)
      else if (sameDigits < 10) simulate(program = program, expectedOutputs = expectedOutputs, current = current + 1000L)
      else simulate(program = program, expectedOutputs = expectedOutputs, current = current + 1)
    }
  }
}
