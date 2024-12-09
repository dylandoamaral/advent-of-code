import scala.io.Source
import scala.util.Using

object Day7Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day7.txt"))(_.getLines().toList).get

    val operations = input.map { case s"$answer: $parameters" => (answer.toLong, parameters.split(" ").map(_.toLong).toList) }
    
    println(concateNumbers(10, 11))

    val answer =
      operations
        .filter { case (answer, parameters) => isOperationValid(answer, parameters) }
        .map(_._1)
        .sum

    print(answer)
  }

  def isOperationValid(answer: Long, parameters: List[Long], currentAnswer: Long = -1): Boolean = {
    if (currentAnswer == answer && parameters.isEmpty) true
    else if (currentAnswer > answer) false
    else if (currentAnswer == -1) isOperationValid(answer, parameters.tail, parameters.head)
    else {
      parameters match
        case head :: tail =>
          isOperationValid(answer, tail, currentAnswer + head)
            || isOperationValid(answer, tail, currentAnswer * head)
            || isOperationValid(answer, tail, concateNumbers(currentAnswer, head))
        case Nil => false
    }
  }

  def concateNumbers(left: Long, right: Long): Long = (left.toString + right.toString).toLong
}
