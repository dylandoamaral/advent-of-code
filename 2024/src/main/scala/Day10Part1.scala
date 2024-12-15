import scala.io.Source
import scala.util.Using

object Day10Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day10.txt"))(_.getLines().toList).get
    val map = input.toArray.map(line => line.toArray.map(_.toString.toInt))

    val answer = computeScore(map)

    print(answer)
  }

  def computeScore(map: Array[Array[Int]]): Int = {
    val size = map.length

    map.head.indices.flatMap { y =>
      map.indices.map { x =>
        if (map(x)(y) == 0) findTrails(map, size, x, y, 0).size
        else 0
      }
    }.sum
  }

  def findTrails(map: Array[Array[Int]], size: Int, x: Int, y: Int, height: Int): Set[(Int, Int)] = {
    if (height == 9) Set((x, y))
    else {
      Set(
        if (x - 1 >= 0 && map(x - 1)(y) == height + 1) findTrails(map, size, x - 1, y, height + 1) else Set.empty,
        if (y - 1 >= 0 && map(x)(y - 1) == height + 1) findTrails(map, size, x, y - 1, height + 1) else Set.empty,
        if (x + 1 < size && map(x + 1)(y) == height + 1) findTrails(map, size, x + 1, y, height + 1) else Set.empty,
        if (y + 1 < size && map(x)(y + 1) == height + 1) findTrails(map, size, x, y + 1, height + 1) else Set.empty
      ).flatten
    }
  }
}
