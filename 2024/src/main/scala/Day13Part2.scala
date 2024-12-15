import scala.io.Source
import scala.util.Using

object Day13Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day13.txt"))(_.getLines().toList).get

    val answer = parse(input.filter(_ != "")).map(_.compute()).collect { case Some(v) => v }.sum

    print(answer)
  }

  def parse(input: List[String], acc: List[Scenario] = List.empty): List[Scenario] = input match
    case a :: b :: p :: tail =>
      val buttonA = a match
        case s"Button A: X+$x, Y+$y" => (x.toLong, y.toLong)
        case _ => ??? // We ignore it
      val buttonB = b match
        case s"Button B: X+$x, Y+$y" => (x.toLong, y.toLong)
        case _ => ??? // We ignore it
      val prize = p match
        case s"Prize: X=$x, Y=$y" => (x.toLong + 10000000000000L, y.toLong + 10000000000000L)
        case _ => ??? // We ignore it

      parse(tail, acc :+ Scenario(buttonA, buttonB, prize))
    case Nil => acc
    case _ => ??? // We ignore it

  case class Scenario(buttonA: (Long, Long), buttonB: (Long, Long), prize: (Long, Long)) {
    def compute(): Option[Long] = {
      val (xa, ya) = buttonA
      val (xb, yb) = buttonB
      val (xp, yp) = prize

      val determinant = xa * yb - ya * xb

      if (determinant == 0) None
      else if ((xp * yb - yp * xb) % determinant != 0 || (yp * xa - xp * ya) % determinant != 0) None
      else {
        val a = (xp * yb - yp * xb) / determinant
        val b = (yp * xa - xp * ya) / determinant

        Some(a * 3 + b)
      }
    }
  }
}
