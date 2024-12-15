import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day04Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day04.txt"))(_.getLines().toList).get
    val pattern: Regex = """XMAS""".r

    val letters = input.toArray.map(_.toCharArray)
    val horizontals = letters.map(_.mkString)
    val verticals = letters.transpose.map(_.mkString)
    val diagonals = {
      val leftToRightDiagonalsTop =
        (0 until letters.head.length - 3).map(column => findDiagonal(letters, column, 0, true).mkString)
      val leftToRightDiagonalsSide =
        (1 until letters.length - 3).map(row => findDiagonal(letters, 0, row, true).mkString)
      val rightToLeftDiagonalsTop =
        (3 until letters.head.length).map(column => findDiagonal(letters, column, 0, false).mkString)
      val rightToLeftDiagonalsSide =
        (1 until letters.length - 3).map(row => findDiagonal(letters, letters.length - 1, row, false).mkString)

      leftToRightDiagonalsTop
        ++ leftToRightDiagonalsSide
        ++ rightToLeftDiagonalsTop
        ++ rightToLeftDiagonalsSide
    }

    val lines =
      horizontals
        ++ horizontals.map(_.reverse)
        ++ verticals
        ++ verticals.map(_.reverse)
        ++ diagonals
        ++ diagonals.map(_.reverse)
    val answer = lines.map(pattern.findAllMatchIn).map(_.size).sum

    print(answer)
  }

  private def findDiagonal(
                            letters: Array[Array[Char]],
                            column: Int,
                            row: Int,
                            leftToRight: Boolean
                          ): Array[Char] = {
    val size =
      if (leftToRight) Math.min(letters.length - column, letters.length - row)
      else Math.min(column + 1, letters.length - row)

    val increaseX: (Int, Int) => Int = (a, b) => if (leftToRight) a + b else a - b

    (0 until size).map(index => letters(row + index)(increaseX(column, index))).toArray
  }

}
