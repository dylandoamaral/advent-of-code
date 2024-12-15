import scala.io.Source
import scala.util.Using

object Day15Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day15.txt"))(_.getLines().toList).get

    val (rawMap, rawMoves) = input.splitAt(input.indexOf(""))
    val map = rawMap.map(_.toArray).toArray.transpose
    val moves = rawMoves.tail.flatten

    val simulation = simulate(map, moves)
    val blocks = simulation.head.indices
      .flatMap(x => simulation.indices.map(y => (x, y, simulation(x)(y))))
      .collect { case (x, y, 'O') => (x, y) }

    val answer = blocks.map { (x, y) => 100 * y + x }.sum

    println(answer)
  }

  def simulate(map: Array[Array[Char]], moves: List[Char]): Array[Array[Char]] = {
    val robotPosition =
      map.head.indices
        .flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, '@') => (x, y) }
        .getOrElse(throw new Exception("Can't find robot"))

    val mapSize = map.head.length

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
              val caseAmount = (0 until robotY).reverse.map(y => map(robotX)(y)).takeWhile(_ == 'O').length

              if (map(robotX)(robotY - caseAmount - 1) == '.') {
                (1 until caseAmount + 1).foreach { x =>
                  acc.map(robotX)(robotY - caseAmount - 1) = 'O'
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX)(robotY - 1) = '@'
                State((robotX, robotY - 1), acc.map)
              }
              else acc
            }
          case '>' =>
            if (robotX + 1 < mapSize && acc.map(robotX + 1)(robotY) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX + 1)(robotY) = '@'
              State((robotX + 1, robotY), acc.map)
            }
            else if (robotX + 1 < mapSize && acc.map(robotX + 1)(robotY) == '#') acc
            else {
              val caseAmount = (robotX + 1 until mapSize).map(x => map(x)(robotY)).takeWhile(_ == 'O').length

              if (map(robotX + caseAmount + 1)(robotY) == '.') {
                (1 until caseAmount + 1).foreach { x =>
                  acc.map(robotX + caseAmount + 1)(robotY) = 'O'
                }
                acc.map(robotX)(robotY) = '.'
                acc.map(robotX + 1)(robotY) = '@'
                State((robotX + 1, robotY), acc.map)
              }
              else acc
            }
          case 'v' =>
            if (robotY + 1 < mapSize && acc.map(robotX)(robotY + 1) == '.') {
              acc.map(robotX)(robotY) = '.'
              acc.map(robotX)(robotY + 1) = '@'
              State((robotX, robotY + 1), acc.map)
            }
            else if (robotY + 1 < mapSize && acc.map(robotX)(robotY + 1) == '#') acc
            else {
              val caseAmount = (robotY + 1 until mapSize).map(y => map(robotX)(y)).takeWhile(_ == 'O').length

              if (map(robotX)(robotY + caseAmount + 1) == '.') {
                (1 until caseAmount + 1).foreach { x =>
                  acc.map(robotX)(robotY + caseAmount + 1) = 'O'
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
              val caseAmount = (0 until robotX).reverse.map(x => map(x)(robotY)).takeWhile(_ == 'O').length

              if (map(robotX - caseAmount - 1)(robotY) == '.') {
                (1 until caseAmount + 1).foreach { x =>
                  acc.map(robotX - caseAmount - 1)(robotY) = 'O'
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
