import scala.io.Source
import scala.util.Using

object Day07Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day07.txt"))(_.getLines().toList).get

    val operations = input.map { case s"$answer: $parameters" => (answer.toLong, parameters.split(" ").map(_.toLong).toList) }

    val answer =
      operations
        .filter { case (answer, parameters) => isOperationValid(answer, parameters) }
        .map(_._1)
        .sum

    print(answer)
  }

  def isOperationValid(answer: Long, parameters: List[Long], currentAnswer: Long = -1): Boolean = {
    if (currentAnswer == answer) true
    else if (currentAnswer > answer) false
    else if (currentAnswer == -1) isOperationValid(answer, parameters.tail, parameters.head)
    else {
      parameters match
        case head :: tail =>
          isOperationValid(answer, tail, currentAnswer + head) || isOperationValid(answer, tail, currentAnswer * head)
        case Nil => false
    }
  }
}
