import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day24Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day24.txt"))(_.getLines().toList).get

    val (rawValues, rawDoors) = input.splitAt(input.indexOf(""))
    val values = rawValues.collect { case s"$id: $value" => (id, value == "1") }.toMap
    val doors = rawDoors.collect { case s"$left $op $right -> $result" => Door(left, right, Operation.parse(op), result) }
    val allValues = findAllValues(values, doors)
    val result = booleansToLong(allValues.toList.filter(_._1.startsWith("z")).sortBy(_._1).map(_._2))

    println(result)
  }

  def booleansToLong(booleans: List[Boolean], acc: Long = 0L, index: Int = 0): Long =
    booleans match
      case head :: next =>
        if (head) booleansToLong(booleans = next, acc = acc + (1L << index), index = index + 1)
        else booleansToLong(booleans = next, acc = acc, index = index + 1)
      case Nil => acc

  def findAllValues(values: Map[String, Boolean], doors: List[Door]): Map[String, Boolean] = {
    var unvisitedDoors = mutable.ListBuffer(doors: _*)
    val allValues = mutable.Map[String, Boolean](values.toList: _*)

    while (unvisitedDoors.exists(_.result.startsWith("z"))) {
      val newUnvisitedDoors = mutable.ListBuffer.empty[Door]

      for (unvisitedDoor <- unvisitedDoors) {
        val maybeResult = for {
          left <- allValues.get(unvisitedDoor.left)
          right <- allValues.get(unvisitedDoor.right)
        } yield unvisitedDoor.operation(left, right)

        maybeResult match
          case Some(result) => allValues(unvisitedDoor.result) = result
          case None         => newUnvisitedDoors += unvisitedDoor
      }

      unvisitedDoors = newUnvisitedDoors
    }

    allValues.toMap
  }

  case class Door(left: String, right: String, operation: Operation, result: String)

  enum Operation {
    case AND
    case XOR
    case OR

    def apply(left: Boolean, right: Boolean): Boolean = this match
      case Operation.AND => left && right
      case Operation.XOR => (left && !right) || (right && !left)
      case Operation.OR  => left || right
  }

  object Operation {
    def parse(string: String): Operation = string match
      case "AND" => Operation.AND
      case "XOR" => Operation.XOR
      case _     => Operation.OR
  }
}
