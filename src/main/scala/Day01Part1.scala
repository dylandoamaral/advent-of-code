import scala.io.Source
import scala.util.Using

object Day01Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day01.txt"))(_.getLines().toList).get

    val locations = input.collect { case s"$left   $right" => (left.toInt, right.toInt) }
    val leftLocations = locations.map(_._1).sorted
    val rightLocations = locations.map(_._2).sorted
    val answer = leftLocations.zip(rightLocations).map((a, b) => Math.abs(a - b)).sum

    print(answer)
  }
}
