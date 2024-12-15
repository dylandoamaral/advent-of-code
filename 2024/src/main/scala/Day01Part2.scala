import scala.io.Source
import scala.util.Using

object Day01Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day01.txt"))(_.getLines().toList).get
    
    val locations = input.collect { case s"$left   $right" => (left.toInt, right.toInt) }
    val leftLocations = locations.map(_._1).groupBy(identity).map((number, list) => (number, list.size))
    val rightLocations = locations.map(_._2).groupBy(identity).map((number, list) => (number, list.size))
    val answer = leftLocations.map((number, count) => number * count * rightLocations.getOrElse(number, 0)).sum
    
    print(answer)
  }
}
