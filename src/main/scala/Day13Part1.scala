import scala.io.Source
import scala.util.Using

object Day13Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day13.txt"))(_.getLines().toList).get

    val answer = parse(input.filter(_ != "")).map(_.compute()).collect { case Some(v) => v }.sum

    print(answer)
  }

  def parse(input: List[String], acc: List[Scenario] = List.empty): List[Scenario] = input match
    case a :: b :: p :: tail =>
      val buttonA = a match
        case s"Button A: X+$x, Y+$y" => (x.toInt, y.toInt)
        case _ => ??? // We ignore it
      val buttonB = b match
        case s"Button B: X+$x, Y+$y" => (x.toInt, y.toInt)
        case _ => ??? // We ignore it
      val prize = p match
        case s"Prize: X=$x, Y=$y" => (x.toInt, y.toInt)
        case _ => ??? // We ignore it

      parse(tail, acc :+ Scenario(buttonA, buttonB, prize))
    case Nil => acc
    case _ => ??? // We ignore it

  case class Scenario(buttonA: (Int, Int), buttonB: (Int, Int), prize: (Int, Int)) {
    def compute(): Option[Int] = {
      val (xa, ya) = (buttonA._1.toFloat, buttonA._2.toFloat)
      val (xb, yb) = (buttonB._1.toFloat, buttonB._2.toFloat)
      val (xp, yp) = (prize._1.toFloat, prize._2.toFloat)

      val b = approximate((yp - ya * xp / xa) / (-1 * ya * xb / xa + yb))
      val a = approximate((xp / xa) - (b * xb / xa))

      if (a < 100 && b < 100 && a.isValidInt && b.isValidInt) Some(3 * a.toInt + b.toInt)
      else None
    }
  }

  def approximate(float: Float): Float = {
    if (float - Math.floor(float).toFloat < 0.0001) Math.floor(float).toFloat
    else if (Math.ceil(float).toFloat - float < 0.0001) Math.ceil(float).toFloat
    else float
  }
}
