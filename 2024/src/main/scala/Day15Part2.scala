import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day15Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day15.txt"))(_.getLines().toList).get

    val (rawMap, rawMoves) = input.splitAt(input.indexOf(""))
    val littleMap = rawMap.map(_.toArray).toArray
    val map = enlarge(littleMap).transpose
    val moves = rawMoves.tail.flatten

    val simulation = simulate(map, moves)
    val blocks = simulation.indices
      .flatMap(x => simulation.head.indices.map(y => (x, y, simulation(x)(y))))
      .collect { case (x, y, '[') => (x, y) }

    val answer = blocks.map { (x, y) => 100 * y + x }.sum

    println(answer)
  }

  def enlarge(map: Array[Array[Char]]): Array[Array[Char]] =
    map.map {
      line => line.toList.flatMap {
        case '#' => "##"
        case '.' => ".."
        case 'O' => "[]"
        case '@' => "@."
      }.toArray
    }

  def caseIsBlocked(leftCaseX: Int, caseY: Int, map: Array[Array[Char]], isUp: Boolean): Boolean = {
    if (isUp && (map(leftCaseX)(caseY - 1) == '#' || map(leftCaseX + 1)(caseY - 1) == '#')) true
    else if (!isUp && (map(leftCaseX)(caseY + 1) == '#' || map(leftCaseX + 1)(caseY + 1) == '#')) true
    else false
  }

  def takeLinkedCases(leftCaseX: Int, caseY: Int, map: Array[Array[Char]], isUp: Boolean): List[(Int, Int)] = {
    if (isUp) {
      val newLinkedCase = mutable.ListBuffer.empty[(Int, Int)]

      if (map(leftCaseX)(caseY - 1) == ']') {
        newLinkedCase ++= takeLinkedCases(leftCaseX - 1, caseY - 1, map, isUp) :+ ((leftCaseX - 1, caseY - 1))
      }
      if (map(leftCaseX + 1)(caseY - 1) == '[') {
        newLinkedCase ++= takeLinkedCases(leftCaseX + 1, caseY - 1, map, isUp) :+ ((leftCaseX + 1, caseY - 1))
      }
      if (map(leftCaseX)(caseY - 1) == '[') {
        newLinkedCase ++= takeLinkedCases(leftCaseX, caseY - 1, map, isUp) :+ ((leftCaseX, caseY - 1))
      }

      newLinkedCase.toList
    } else {
      val newLinkedCase = mutable.ListBuffer.empty[(Int, Int)]

      if (map(leftCaseX)(caseY + 1) == ']') {
        newLinkedCase ++= takeLinkedCases(leftCaseX - 1, caseY + 1, map, isUp) :+ ((leftCaseX - 1, caseY + 1))
      }
      if (map(leftCaseX + 1)(caseY + 1) == '[') {
        newLinkedCase ++= takeLinkedCases(leftCaseX + 1, caseY + 1, map, isUp) :+ ((leftCaseX + 1, caseY + 1))
      }
      if (map(leftCaseX)(caseY + 1) == '[') {
        newLinkedCase ++= takeLinkedCases(leftCaseX, caseY + 1, map, isUp) :+ ((leftCaseX, caseY + 1))
      }

      newLinkedCase.toList
    }
  }

  def simulate(map: Array[Array[Char]], moves: List[Char]): Array[Array[Char]] = {
    val mapX = map.length
    val mapY = map.head.length

    val robotPosition =
      map.indices
        .flatMap(x => map.head.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, '@') => (x, y) }
        .getOrElse(throw new Exception("Can't find robot"))

    moves.foldLeft(State(robotPosition, map)) {
      case (acc, curr) =>
        val robotX = acc.robotPosition._1
        val robotY = acc.robotPosition._2

        curr match
          case '^' =>
            if (robotY - 1 >= 0 && acc.map(robotX)(robotY - 1) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX)(robotY - 1) = '@'
              State((robotX, robotY - 1), acc.map)
            }
            else if (robotY - 1 >= 0 && acc.map(robotX)(robotY - 1) == '#') acc
            else {
              val pushRight = acc.map(robotX)(robotY - 1) == ']'

              val leftCaseX = if (pushRight) robotX - 1 else robotX
              val rightCaseX = if (pushRight) robotX else robotX + 1
              val caseY = robotY - 1
              val isUp = true

              val linkedCases = takeLinkedCases(leftCaseX, caseY, map, isUp) :+ (leftCaseX, caseY)
              val casesCanMove = linkedCases.map((x, y) => caseIsBlocked(x, y, map, isUp)).forall(_ == false)

              if (casesCanMove) {
                linkedCases.sortBy((x, y) => y).foreach { case (x, y) =>
                  acc.map(x)(y - 1) = '['
                  acc.map(x + 1)(y - 1) = ']'
                  acc.map(x)(y) = '.'
                  acc.map(x + 1)(y) = '.'
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX)(robotY - 1) = '@'
                State((robotX, robotY - 1), acc.map)
              }
              else acc
            }
          case '>' =>
            if (robotX + 1 < mapX && acc.map(robotX + 1)(robotY) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX + 1)(robotY) = '@'
              State((robotX + 1, robotY), acc.map)
            }
            else if (robotX + 1 < mapX && acc.map(robotX + 1)(robotY) == '#') acc
            else {
              val caseAmount = (robotX + 1 until mapX).map(x => map(x)(robotY)).takeWhile("[]".contains(_)).length

              if (map(robotX + caseAmount + 1)(robotY) == '.') {
                (1 until caseAmount + 1).reverse.foreach { x =>
                  acc.map(robotX + x + 1)(robotY) = acc.map(robotX + x)(robotY)
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX + 1)(robotY) = '@'
                State((robotX + 1, robotY), acc.map)
              }
              else acc
            }
          case 'v' =>
            if (robotY + 1 < mapY && acc.map(robotX)(robotY + 1) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX)(robotY + 1) = '@'
              State((robotX, robotY + 1), acc.map)
            }
            else if (robotY + 1 < mapY && acc.map(robotX)(robotY + 1) == '#') acc
            else {
              val pushRight = acc.map(robotX)(robotY + 1) == ']'

              val leftCaseX = if (pushRight) robotX - 1 else robotX
              val rightCaseX = if (pushRight) robotX else robotX + 1
              val caseY = robotY + 1
              val isUp = false

              val linkedCases = takeLinkedCases(leftCaseX, caseY, map, isUp) :+ (leftCaseX, caseY)
              val casesCanMove = linkedCases.map((x, y) => caseIsBlocked(x, y, map, isUp)).forall(_ == false)

              if (casesCanMove) {
                linkedCases.sortBy((x, y) => y)(Ordering.Int.reverse).foreach { case (x, y) =>
                  acc.map(x)(y + 1) = '['
                  acc.map(x + 1)(y + 1) = ']'
                  acc.map(x)(y) = '.'
                  acc.map(x + 1)(y) = '.'
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX)(robotY + 1) = '@'
                State((robotX, robotY + 1), acc.map)
              }
              else acc
            }
          case '<' =>
            if (robotX - 1 >= 0 && acc.map(robotX - 1)(robotY) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX - 1)(robotY) = '@'
              State((robotX - 1, robotY), acc.map)
            }
            else if (robotX - 1 >= 0 && acc.map(robotX - 1)(robotY) == '#') acc
            else {
              val caseAmount = (0 until robotX).reverse.map(x => map(x)(robotY)).takeWhile("[]".contains(_)).length

              if (map(robotX - caseAmount - 1)(robotY) == '.') {
                (1 until caseAmount + 1).reverse.foreach { x =>
                  acc.map(robotX - x - 1)(robotY) = acc.map(robotX - x)(robotY)
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX - 1)(robotY) = '@'
                State((robotX - 1, robotY), acc.map)
              }
              else acc
            }
    }.map
  }

  case class State(robotPosition: (Int, Int), map: Array[Array[Char]])
}
