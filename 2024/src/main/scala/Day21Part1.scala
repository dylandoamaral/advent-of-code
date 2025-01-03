import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21Part1 {
  def main(args: Array[String]): Unit = {
    val codes = Using(Source.fromResource("Day21.txt"))(_.getLines().toList).get

    val answer = codes.map { code =>
      val numericPart = code.dropRight(1).toInt
      val shortestPathLength = shortestPathOf(code).length

      numericPart * shortestPathLength
    }.sum

    print(answer)
  }

  def shortestPathOf(
      code: String,
      numericKeypad: NumericKeypad = NumericKeypad(),
      directionalKeypad1: DirectionalKeypad = DirectionalKeypad(),
      directionalKeypad2: DirectionalKeypad = DirectionalKeypad()
  ): List[Command] = {
    val paths: Set[List[Command]] = pathsOf(code, numericKeypad, directionalKeypad1, directionalKeypad2)

    paths.minBy(_.size)
  }

  def pathsOf(
      code: String,
      numericKeypad: NumericKeypad,
      directionalKeypad1: DirectionalKeypad,
      directionalKeypad2: DirectionalKeypad
  ): Set[List[Command]] = {
    val numericKeypadPossibilities = numericKeypad.goToAll(code.toList)
    val directionalKeypad1Possibilities = numericKeypadPossibilities.map(directionalKeypad1.goToAll).reduce(_ ++ _)
    val directionalKeypad2Possibilities = directionalKeypad1Possibilities.map(directionalKeypad2.goToAll).reduce(_ ++ _)

    directionalKeypad2Possibilities
  }

  object Command {
    def findDirection(fromPosition: (Int, Int), toPosition: (Int, Int)) = {
      if (fromPosition._1 < toPosition._1) Command.Bottom
      else if (fromPosition._1 > toPosition._1) Command.Top
      else if (fromPosition._2 < toPosition._2) Command.Right
      else if (fromPosition._2 > toPosition._2) Command.Left
      else throw new Exception("Both positions are the same.")
    }
  }

  enum Command {
    case Left
    case Right
    case Top
    case Bottom
    case A
  }

  object NumericKeypad {
    val _7: (Int, Int) = (0, 0)
    val _8: (Int, Int) = (0, 1)
    val _9: (Int, Int) = (0, 2)
    val _4: (Int, Int) = (1, 0)
    val _5: (Int, Int) = (1, 1)
    val _6: (Int, Int) = (1, 2)
    val _1: (Int, Int) = (2, 0)
    val _2: (Int, Int) = (2, 1)
    val _3: (Int, Int) = (2, 2)
    val _0: (Int, Int) = (3, 2)
    val A: (Int, Int) = (3, 3)

    def fromCharacter(char: Char): (Int, Int) = char match
      case '7' => _7
      case '8' => _8
      case '9' => _9
      case '4' => _4
      case '5' => _5
      case '6' => _6
      case '1' => _1
      case '2' => _2
      case '3' => _3
      case '0' => _0
      case 'A' => A
      case _   => throw new Exception(s"Char $char not expected.")
  }

  case class NumericKeypad(currentPosition: (Int, Int) = NumericKeypad.A) {
    import NumericKeypad._

    def goTo(targetPosition: (Int, Int)): Set[List[Command]] = {
      val commands = prunePossibilities(goToRec(currentPosition, targetPosition, Set.empty))

      if (commands.isEmpty) Set(List(Command.A))
      else commands.map(_ :+ Command.A)
    }

    def goToAll(characters: List[Char]): Set[List[Command]] = {
      case class State(commands: Set[List[Command]], keypad: NumericKeypad)

      characters
        .foldLeft(State(Set.empty, this)) { case (state, curr) =>
          val targetPosition = NumericKeypad.fromCharacter(curr)

          val nextCommands = state.keypad.goTo(targetPosition)

          val newCommands =
            if (state.commands.isEmpty) nextCommands
            else {
              for {
                commands <- state.commands
                adds <- nextCommands
              } yield commands ++ adds
            }

          State(newCommands, state.keypad.copy(currentPosition = targetPosition))
        }
        .commands
    }

    def goToRec(focusPosition: (Int, Int), targetPosition: (Int, Int), acc: Set[List[Command]]): Set[List[Command]] = {
      val distance = manhattanDistance(focusPosition, targetPosition)

      if (distance == 0) acc
      else {
        val neighbourPositions = focusPosition match
          case NumericKeypad._7 => Set(_8, _4)
          case NumericKeypad._8 => Set(_7, _5, _9)
          case NumericKeypad._9 => Set(_8, _6)
          case NumericKeypad._4 => Set(_7, _5, NumericKeypad._1)
          case NumericKeypad._5 => Set(_8, _6, _2, _4)
          case NumericKeypad._6 => Set(_9, _5, _3)
          case NumericKeypad._1 => Set(_4, _2)
          case NumericKeypad._2 => Set(NumericKeypad._1, _5, _3, _0)
          case NumericKeypad._3 => Set(_6, _2, A)
          case NumericKeypad._0 => Set(_2, A)
          case NumericKeypad.A  => Set(_3, _0)
          case _                => throw new Exception(s"FocusPosition $focusPosition should not exist.")

        neighbourPositions
          .filter(position => manhattanDistance(position, targetPosition) < distance)
          .map(position => {
            val direction = Command.findDirection(focusPosition, position)
            goToRec(position, targetPosition, if (acc.isEmpty) Set(List(direction)) else acc.map(_ :+ direction))
          })
          .foldLeft(Set.empty)(_ ++ _)
      }
    }
  }

  object DirectionalKeypad {
    val top: (Int, Int) = (0, 1)
    val right: (Int, Int) = (1, 2)
    val bottom: (Int, Int) = (1, 1)
    val left: (Int, Int) = (1, 0)
    val A: (Int, Int) = (0, 2)

    def fromCommand(command: Command): (Int, Int) = command match
      case Command.Left   => left
      case Command.Right  => right
      case Command.Top    => top
      case Command.Bottom => bottom
      case Command.A      => A
  }

  case class DirectionalKeypad(currentPosition: (Int, Int) = DirectionalKeypad.A) {
    import DirectionalKeypad._

    val computations: mutable.Map[((Int, Int), (Int, Int)), Set[List[Command]]] = mutable.Map.empty

    def goTo(targetPosition: (Int, Int)): Set[List[Command]] = {
      val key = (currentPosition, targetPosition)

      computations.get(key) match
        case Some(value) => value
        case None =>
          val commands = prunePossibilities(goToRec(currentPosition, targetPosition, Set.empty))
          val result = if (commands.isEmpty) Set(List(Command.A)) else commands.map(_ :+ Command.A)

          computations(key) = result

          result
    }

    def goToAll(commands: List[Command]): Set[List[Command]] = {
      case class State(commands: Set[List[Command]], keypad: DirectionalKeypad)

      commands
        .foldLeft(State(Set.empty, this)) { case (state, curr) =>
          val targetPosition = fromCommand(curr)

          val nextCommands = state.keypad.goTo(targetPosition)

          val newCommands =
            if (state.commands.isEmpty) nextCommands
            else {
              for {
                commands <- state.commands
                adds <- nextCommands
              } yield commands ++ adds
            }

          State(newCommands, state.keypad.copy(currentPosition = targetPosition))
        }
        .commands
    }

    final def goToRec(focusPosition: (Int, Int), targetPosition: (Int, Int), acc: Set[List[Command]]): Set[List[Command]] = {
      val distance = manhattanDistance(focusPosition, targetPosition)

      if (distance == 0) acc
      else {
        val neighbourPositions = focusPosition match
          case DirectionalKeypad.top    => Set(A, bottom)
          case DirectionalKeypad.right  => Set(A, bottom)
          case DirectionalKeypad.bottom => Set(top, right, left)
          case DirectionalKeypad.left   => Set(bottom)
          case DirectionalKeypad.A      => Set(top, right)
          case _                        => throw new Exception(s"FocusPosition $focusPosition should not exist.")

        neighbourPositions
          .filter(position => manhattanDistance(position, targetPosition) < distance)
          .flatMap(position => {
            val direction = Command.findDirection(focusPosition, position)
            goToRec(position, targetPosition, if (acc.isEmpty) Set(List(direction)) else acc.map(_ :+ direction))
          })
      }
    }
  }

  def prunePossibilities(possibilities: Set[List[Command]]): Set[List[Command]] = {
    @tailrec
    def possibilityIsConsecutive(
        commands: List[Command],
        commandsDone: Set[Command] = Set.empty,
        previousCommand: Option[Command] = None
    ): Boolean =
      commands match
        case head :: next if !previousCommand.contains(head) && commandsDone.contains(head) => false
        case head :: next => possibilityIsConsecutive(next, commandsDone + head, Some(head))
        case Nil          => true

    val filteredPossibilities = possibilities.filter(possibilityIsConsecutive(_))

    if (filteredPossibilities.isEmpty) possibilities
    else filteredPossibilities
  }

  def manhattanDistance(pos1: (Int, Int), pos2: (Int, Int)): Int =
    Math.abs(pos1._1 - pos2._1) + Math.abs(pos1._2 - pos2._2)
}
