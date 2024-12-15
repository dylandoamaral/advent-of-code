import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day03Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day03.txt"))(_.getLines().toList).get.mkString("")
    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val answer = pattern
      .findAllMatchIn(input)
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum

    print(answer)
  }
}
