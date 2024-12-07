import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day3Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day3.txt"))(_.getLines().toList).get.mkString("")
    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val answer =
      input
        .split("""do\(\)""")
        .map(subinput => subinput.split("""don't\(\)"""))
        .map(parts =>
            pattern
              .findAllMatchIn(parts.head)
              .map(m => m.group(1).toInt * m.group(2).toInt)
              .sum
          )
        .sum

    print(answer)
  }
}
