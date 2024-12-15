import scala.io.Source
import scala.util.Using

object Day14Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day14.txt"))(_.getLines().toList).get
    val roomSize = (101, 103)
    val maxSeconds = 10000

    val robots = input.map(Robot.parse)
    val pairs =
      (0 to maxSeconds)
        .map(second => (second, robots.map(_.findFinalPosition(roomSize, second))))
        .map((second, positions) => (second, buildRoom(positions, roomSize)))
        .filter((_, room) => containACenteredTriangle(room, roomSize))

    pairs.foreach((second, room) =>
      println(second)
      show(room, roomSize)
      println("===================================================================================================================================================================================================================")
    )
  }

  def buildRoom(positions: List[(Int, Int)], roomSize: (Int, Int)): Array[Array[Boolean]] = {
    val array = Array.fill(roomSize._1, roomSize._2)(false)
    positions.foreach { case (x, y) => array(x)(y) = true }
    array
  }

  def show(room: Array[Array[Boolean]], roomSize: (Int, Int)): Unit = {
    room.foreach { x =>
      x.foreach { y =>
        if (y) print("O") else print("_")
      }
      println("")
    }
  }

  def containACenteredTriangle(room: Array[Array[Boolean]], roomSize: (Int, Int)): Boolean = {
    val center = roomSize._1 / 2 + 1
    (0 to roomSize._2 - 3).map(y =>
      room(center)(y) &&
        room(center - 1)(y) && room(center)(y) && room(center + 1)(y) &&
        room(center - 2)(y) && room(center - 1)(y) && room(center)(y) && room(center + 1)(y) && room(center + 2)(y)
    ).exists(identity)
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
