import Day6Part1.Case.Guard

import scala.annotation.tailrec
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Using

object Day6Part1 {
  enum Direction {
    case Top
    case Bottom
    case Right
    case Left

    def toRaw: String = this match
      case Direction.Top => "^"
      case Direction.Bottom => "v"
      case Direction.Right => ">"
      case Direction.Left => "<"

    def turn(): Direction = this match
      case Direction.Top => Direction.Right
      case Direction.Bottom => Direction.Left
      case Direction.Right => Direction.Bottom
      case Direction.Left => Direction.Top
  }

  enum Case {
    case Guard(direction: Direction)
    case Crate
    case Path

    def toRaw: String = this match
      case Case.Guard(direction) => direction.toRaw
      case Case.Crate => "#"
      case Case.Path => "."

    def isGuard: Boolean = this match
      case _: Case.Guard => true
      case _ => false

    def isCrate: Boolean = this match
      case Case.Crate => true
      case _ => false
  }

  implicit class arrayOfArrayOps[A: ClassTag](array: Array[Array[A]]) {
    def update(x: Int, y: Int, newValue: A): Array[Array[A]] = {
      array.zipWithIndex.map { case (row, rowIndex) =>
        if (rowIndex == x)
          row.zipWithIndex.map { case (value, colIndex) =>
            if (colIndex == y) newValue else value
          }
        else row
      }
    }
  }

  case class Board(data: Array[Array[Case]], visitedCases: Set[(Int, Int)] = Set.empty) {
    @tailrec
    final def move(): Board = {
      val (x, y, guard) = findGuardPosition()

      if (isGuardReachedBorder((x, y))) this
      else {
        val (dx, dy) = guard.direction match {
          case Direction.Top => (0, 1)
          case Direction.Bottom => (0, -1)
          case Direction.Right => (1, 0)
          case Direction.Left => (-1, 0)
        }

        val newX = x - dy
        val newY = y + dx

        if (data(newX)(newY).isCrate) {
          copy(data = data.update(x, y, guard.copy(direction = guard.direction.turn()))).move()
        } else {
          copy(data = data.update(x, y, Case.Path).update(newX, newY, guard), visitedCases = visitedCases + ((x, y))).move()
        }
      }
    }


    def findGuardPosition(): (Int, Int, Case.Guard) = {
      data.head.indices
        .flatMap(x => data.indices.map(y => (x, y, data(x)(y))))
        .collectFirst { case (x, y, guard: Case.Guard) => (x, y, guard) }
        .getOrElse(throw new Exception("Can't find guard"))
    }

    def isGuardReachedBorder(guardPosition: (Int, Int)): Boolean = {
      guardPosition._1 == 0
        || guardPosition._2 == 0
        || guardPosition._1 == data.head.length - 1
        || guardPosition._2 == data.length - 1
    }

    def show(): Unit = data.foreach(a => println(a.map(a => a.toRaw).mkString))
  }

  object Board {
    def fromLines(lines: List[String]): Board = Board(
      lines.toArray.map(line =>
        line.collect {
          case '#' => Case.Crate
          case '.' => Case.Path
          case '^' => Case.Guard(Direction.Top)
          case 'v' => Case.Guard(Direction.Bottom)
          case '>' => Case.Guard(Direction.Right)
          case '<' => Case.Guard(Direction.Left)
        }.toArray
      )
    )
  }


  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day6.txt"))(_.getLines().toList).get
    val board = Board.fromLines(input)

    val answer = board.move().visitedCases.size + 1

    print(answer)
  }

}
