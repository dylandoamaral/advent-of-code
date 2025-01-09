import scala.io.Source
import scala.util.Using

object Day24Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day24.txt"))(_.getLines().toList).get

    val (rawValues, rawDoors) = input.splitAt(input.indexOf(""))
    val values = rawValues.collect { case s"$id: $value" => (id, value == "1") }.toMap
    val doors = rawDoors.collect { case s"$left $op $right -> $result" => Door(left, right, Operation.parse(op), result) }
    val mapping = doors.groupBy(_.result).view.mapValues(_.head).toMap

    val result = doors
      .filter(_.result.startsWith("z"))
      .map(_.result)
      .map(z => bitIsOk(z, mapping))
      .collect { case Left(errors) => errors }
      .reduce(_ ++ _)
      .toList
      .sorted
      .mkString(",")

    println(result)
  }

  def bitIsOk(z: String, mapping: Map[String, Door]): Either[Set[String], Unit] = {
    if (z == "z00" || z == "z01" || z == "z45") Right(())
    else {
      val door = mapping(z)

      if (!mapping.contains(door.left)) Left(Set(z))
      else {
        val childDoor = if (mapping(door.left).operands.contains(z.replace("z", "x"))) mapping(door.left) else mapping(door.right)
        val otherDoor = if (mapping(door.left).operands.contains(z.replace("z", "x"))) mapping(door.right) else mapping(door.left)

        val otherChildDoor =
          if (mapping(otherDoor.left).operands.contains(z.replace("z", "x"))) mapping(otherDoor.left) else mapping(otherDoor.right)
        val otherOtherDoor =
          if (mapping(otherDoor.left).operands.contains(z.replace("z", "x"))) mapping(otherDoor.right) else mapping(otherDoor.left)

        val error1 = if (mapping(z).operation == Operation.XOR) Set.empty else Set(mapping(z).result)
        val error2 = if (childDoor.operation == Operation.XOR) Set.empty else Set(childDoor.result)
        val error3 = if (otherDoor.operation == Operation.OR) Set.empty else Set(otherDoor.result)
        val error4 = if (otherChildDoor.operation == Operation.AND || error3.nonEmpty) Set.empty else Set(otherChildDoor.result)
        val error5 = if (otherOtherDoor.operation == Operation.AND || error3.nonEmpty) Set.empty else Set(otherOtherDoor.result)

        val errors = error1 ++ error2 ++ error3 ++ error4 ++ error5

        if (errors.isEmpty) Right(())
        else if (error1.nonEmpty) Left(error1)
        else Left(errors)
      }
    }
  }

  case class Door(left: String, right: String, operation: Operation, result: String) {
    def operands: List[String] = List(left, right)
  }

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

    implicit val ordering: Ordering[Operation] = Ordering[Int].on {
      case Operation.AND => 0
      case Operation.XOR => 1
      case Operation.OR  => 2
    }
  }
}
