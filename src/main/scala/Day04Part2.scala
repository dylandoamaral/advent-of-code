import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day04Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day04.txt"))(_.getLines().toList).get
    val pattern: Regex = """XMAS""".r

    val letters = input.toArray.map(_.toCharArray)

    val answer = {
      for {
        x <- (0 until letters.head.length - 2)
        y <- (0 until letters.length - 2)
      } yield isXMAX(letters, x, y)
    }.count(_ == true)

    print(answer)
  }

  private def isXMAX(letters: Array[Array[Char]], x: Int, y: Int): Boolean = {
    val isLeftXmas = letters(y)(x) == 'M' && letters(y + 1)(x + 1) == 'A' && letters(y + 2)(x + 2) == 'S'
    val isLeftXmasReversed = letters(y)(x) == 'S' && letters(y + 1)(x + 1) == 'A' && letters(y + 2)(x + 2) == 'M'
    val isRightXmas = letters(y)(x + 2) == 'M' && letters(y + 1)(x + 1) == 'A' && letters(y + 2)(x) == 'S'
    val isRightXmasReversed = letters(y)(x + 2) == 'S' && letters(y + 1)(x + 1) == 'A' && letters(y + 2)(x) == 'M'

    (isLeftXmas || isLeftXmasReversed) && (isRightXmas || isRightXmasReversed)
  }
}
