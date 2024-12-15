import scala.io.Source
import scala.util.Using

import scala.collection.mutable

object Day11Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day11.txt"))(_.getLines().toList).get
    val stones = input.head.split(" ").map(_.toLong).toList

    val answer = blink(stones = stones, blinking = 75)

    print(answer)
  }

  def blink(stones: List[Long], blinking: Int): Long = {
    val computations = mutable.Map.empty[(Long, Int), Long]

    stones.map(stone => blinkOneStone(stone, blinking, computations)).sum
  }

  def blinkOneStone(stone: Long, blinking: Int, computations: mutable.Map[(Long, Int), Long]): Long = {
    if (blinking == 0) 1
    else {
      computations.get((stone, blinking)) match
        case Some(count) => count
        case None =>
          val string = stone.toString
          val length = string.length

          val result =
            if (stone == 0) blinkOneStone(1, blinking - 1, computations)
            else if (length % 2 == 0)
              blinkOneStone(string.take(length / 2).toInt, blinking - 1, computations) +
                blinkOneStone(string.drop(length / 2).toInt, blinking - 1, computations)
            else blinkOneStone(stone * 2024, blinking - 1, computations)

          computations((stone, blinking)) = result

          result
    }
  }
}
