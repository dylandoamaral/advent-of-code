import scala.io.Source
import scala.util.Using

object Day17Part1 {

  case class Program (registerA: Int, registerB: Int, registerC: Int, outputs: List[Int], instructionPointer: Int,  instructions: Array[Int]) {
    def run(): List[Int] =
      if (isFinished) outputs
      else runOneInstruction().run()

    def runOneInstruction(): Program = {
      val instruction = instructions(instructionPointer)
      val literalOperand = instructions(instructionPointer + 1)
      val comboOperand = literalOperand match
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => 3
        case 4 => registerA
        case 5 => registerB
        case 6 => registerC
        case 7 => throw new Exception("Combo operand 7 is reserved and will not appear in valid programs.")

      instruction match
        case 0 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerA = (numerator / denominator).toInt, instructionPointer = instructionPointer + 2)
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
          this.copy(outputs = outputs :+ comboOperand % 8, instructionPointer = instructionPointer + 2)
        case 6 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerB = (numerator / denominator).toInt, instructionPointer = instructionPointer + 2)
        case 7 =>
          val numerator = registerA
          val denominator = Math.pow(2, comboOperand)

          this.copy(registerC = (numerator / denominator).toInt, instructionPointer = instructionPointer + 2)
    }

    def isFinished: Boolean = instructionPointer >= instructions.length
  }

  object Program {
    def parse(lines: Array[String]): Program = Program(
      registerA = lines(0).dropWhile(!_.isDigit).toInt,
      registerB = lines(1).dropWhile(!_.isDigit).toInt,
      registerC = lines(2).dropWhile(!_.isDigit).toInt,
      outputs = List.empty,
      instructionPointer = 0,
      instructions = lines(4).dropWhile(!_.isDigit).split(",").map(_.toInt),
    )
  }

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day17.txt"))(_.getLines().toArray).get

    val answer = Program.parse(input).run().mkString(",")
    print(answer)
  }
}
