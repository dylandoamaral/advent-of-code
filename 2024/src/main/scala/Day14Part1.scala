import scala.io.Source
import scala.util.Using

object Day14Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day14.txt"))(_.getLines().toList).get
    val roomSize = (101, 103)
    val seconds = 100

    val robots = input.map(Robot.parse)
    val positions = robots.map(_.findFinalPosition(roomSize, seconds))

    val answer = positions.foldLeft((0, 0, 0, 0)) {
      case ((topLeft, topRight, bottomLeft, bottomRight), (x, y)) =>
        if (x < roomSize._1 / 2 && y < roomSize._2 / 2) (topLeft + 1, topRight, bottomLeft, bottomRight)
        else if (x > roomSize._1 / 2 && y < roomSize._2 / 2) (topLeft, topRight + 1, bottomLeft, bottomRight)
        else if (x < roomSize._1 / 2 && y > roomSize._2 / 2) (topLeft, topRight, bottomLeft + 1, bottomRight)
        else if (x > roomSize._1 / 2 && y > roomSize._2 / 2) (topLeft, topRight, bottomLeft, bottomRight + 1)
        else (topLeft, topRight, bottomLeft, bottomRight)
    }

    println(answer._1 * answer._2 * answer._3 * answer._4)
  }

  case class Robot(position: (Int, Int), velocity: (Int, Int)) {
    def findFinalPosition(roomSize: (Int, Int), seconds: Int): (Int, Int) = {
      val rawFinalX = (position._1 + velocity._1 * seconds) % roomSize._1
      val rawFinalY = (position._2 + velocity._2 * seconds) % roomSize._2

      val finalX = if (rawFinalX >= 0) rawFinalX else roomSize._1 + rawFinalX
      val finalY = if (rawFinalY >= 0) rawFinalY else roomSize._2 + rawFinalY

      (finalX, finalY)
    }
  }

  object Robot {
    def parse(string: String): Robot = string match
      case s"p=$x,$y v=$vx,$vy" => Robot((x.toInt, y.toInt), (vx.toInt, vy.toInt))
      case _ => throw new Exception(s"Can't parse '$string'")
  }
}
