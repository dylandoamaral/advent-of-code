import scala.io.Source
import scala.util.Using

object Day8Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day8.txt"))(_.getLines().toList).get

    val grid = input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map { case (character, x) => (x, y, character) }
    }

    val answer = foundAntinodes(grid, input.head.length).size

    print(answer)
  }

  def foundAntinodes(grid: List[(Int, Int, Char)], width: Int, antinodes: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] = {
    grid match
      case (_, _, char) :: tail if char == '.' => foundAntinodes(tail, width, antinodes)
      case (x, y, char) :: tail =>
        val newAntinodes =
          tail
            .filter { case (_, _, otherChar) => otherChar == char}
            .flatMap {
              case (otherX, otherY, otherChar) =>
                val (dx, dy) = (otherX - x, otherY - y)
                (0 until 100).flatMap(factor => List((x - factor * dx, y - factor * dy), (otherX + factor * dx, otherY + factor * dy)))
            }
            .filter { case (finalX, finalY) => finalX >= 0 && finalY >= 0 && finalX < width && finalY < width}

        foundAntinodes(tail, width, antinodes ++ newAntinodes)
      case Nil => antinodes
  }
}
