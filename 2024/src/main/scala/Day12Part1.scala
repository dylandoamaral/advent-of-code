import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day12Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day12.txt"))(_.getLines().toList).get
    val plots = input.toArray.map(_.toArray).transpose
    val size = plots.length

    val regions = findRegions(plots, size)
    val answer = regions.map((x, y) => {calculateRegion(x, y, plots, size)}).map(_ * _).sum

    println(answer)
  }

  def findRegions(plots: Array[Array[Char]], size: Int): List[(Int, Int)] = {
    plots.indices.flatMap { x => plots(x).indices.map { y => (x, y) } }
      .filter {
        case (x, y) =>
          val leftIsNotSame = x == 0 || plots(x - 1)(y) != plots(x)(y)
          val topIsNotSame = y == 0 || plots(x)(y - 1) != plots(x)(y)

          leftIsNotSame && !hasSomethingLeftAbove(x, y, x, y, plots, size)
      }
      .toList
  }

  def hasSomethingLeftAbove(x: Int, y: Int, startX: Int, startY: Int, plots: Array[Array[Char]], size: Int,  visited: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer.empty[(Int, Int)]): Boolean = {
    if (visited.contains((x, y))) false
    else if (y - 1 >= 0 && plots(x)(y - 1) == plots(x)(y) && y - 1 == startY && x < startX) true
    else if (y - 1 >= 0 && plots(x)(y - 1) == plots(x)(y) && y - 1 == startY - 1) true
    else {
      visited += ((x, y))

      val top = if (y - 1 >= 0 && plots(x)(y - 1) == plots(x)(y)) hasSomethingLeftAbove(x, y - 1, startX, startY, plots, size, visited) else false
      val bottom = if (y + 1 < size && plots(x)(y + 1) == plots(x)(y)) hasSomethingLeftAbove(x, y + 1, startX, startY, plots, size, visited) else false
      val left = if (x - 1 >= 0 && plots(x - 1)(y) == plots(x)(y)) hasSomethingLeftAbove(x - 1, y, startX, startY, plots, size, visited) else false
      val right =  if (x + 1 < size && plots(x + 1)(y) == plots(x)(y)) hasSomethingLeftAbove(x + 1, y, startX, startY, plots, size, visited) else false

      left || right || bottom || top
    }
  }


  def calculateRegion(x: Int, y: Int, plots: Array[Array[Char]], size: Int, visited: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer.empty[(Int, Int)]): (Int, Int) = {
    val plot = plots(x)(y)
    val fences = calculateFence(x, y, plots, size)

    visited += ((x, y))

    val restOfPlot = List(
      if (x - 1 >= 0 && !visited.contains(x - 1, y) && plots(x - 1)(y) == plot) calculateRegion(x - 1, y, plots, size, visited) else (0, 0),
      if (y - 1 >= 0 && !visited.contains(x, y - 1) && plots(x)(y - 1) == plot) calculateRegion(x, y - 1, plots, size, visited) else (0, 0),
      if (x + 1 < size && !visited.contains(x + 1, y) && plots(x + 1)(y) == plot) calculateRegion(x + 1, y, plots, size, visited) else (0, 0),
      if (y + 1 < size && !visited.contains(x, y + 1) && plots(x)(y + 1) == plot) calculateRegion(x, y + 1, plots, size, visited) else (0, 0),
    ).reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    (restOfPlot._1 + fences, restOfPlot._2 + 1)
  }

  def calculateFence(x: Int, y: Int, plots: Array[Array[Char]], size: Int): Int = {
    val plot = plots(x)(y)

    val answer = List(
      if (x - 1 == -1) 1 else if (plots(x - 1)(y) != plot) 1 else 0,
      if (y - 1 == -1) 1 else if (plots(x)(y - 1) != plot) 1 else 0,
      if (x + 1 == size) 1 else if (plots(x + 1)(y) != plot) 1 else 0,
      if (y + 1 == size) 1 else if (plots(x)(y + 1) != plot) 1 else 0
    ).sum

    answer
  }
}
