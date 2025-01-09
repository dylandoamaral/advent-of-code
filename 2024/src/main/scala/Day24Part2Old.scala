import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day24Part2Old {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day24.txt"))(_.getLines().toList).get

    val (rawValues, rawDoors) = input.splitAt(input.indexOf(""))
    val values = rawValues.collect { case s"$id: $value" => (id, value == "1") }.toMap
    val doors = rawDoors.collect { case s"$left $op $right -> $result" => Door(left, right, Operation.parse(op), result) }

    val xNumber = numberOf("x", values)
    val yNumber = numberOf("y", values)
    val expectedZNumber = xNumber & yNumber

    val allValues = findAllValues(values, doors)
    val receivedZNumber = numberOf("z", allValues)

    val expectedZBits = longToBooleans(expectedZNumber)
    val receivedZBitsRaw = longToBooleans(receivedZNumber)
    val receivedZBits = List.fill(expectedZBits.length - receivedZBitsRaw.length)(false) ++ receivedZBitsRaw

    println(xNumber)
    println(yNumber)
    println(expectedZNumber)
    println(receivedZNumber)
    println(expectedZBits)
    println(receivedZBits)

    val wrongBits = expectedZBits
      .zip(receivedZBits)
      .zipWithIndex
      .filter(res => res._1._1 != res._1._2)
      .map(_._2)
      .map(z => if (z < 10) "z0" + z else "z" + z)

    println(wrongBits)

    // println(findPossibleSwap(doors))

    val wrongDoors = doors.filter(door => wrongBits.contains(door.result))
    val possibleSwap = wrongDoors.flatMap(door => List(door.left, door.right)).toSet

    for (swap <- possibleSwap) {
      val test = findDependencesOf(swap, doors)

      if (test.forall(wrongDoors.map(_.result).contains(_))) {
        println(s"$swap $test")
      }
    }
  }

  def findDependencesOf(value: String, doors: List[Door], acc: Set[String] = Set.empty): Set[String] =
    doors.filter(door => door.left == value || door.right == value).foldLeft(Set.empty[String]) { case (acc, curr) =>
      if (curr.result.startsWith("z")) acc + curr.result
      else acc ++ findDependencesOf(curr.result, doors)
    }

  def findPossibleSwap1(doors: List[Door], acc: List[(Swap, Swap, Swap, Swap)]): List[(Swap, Swap, Swap, Swap)] =
    doors match
      case head :: next =>
        val swap1 = ???
        ???
      case Nil => acc

  def numberOf(letter: String, values: Map[String, Boolean]): Long =
    booleansToLong(values.toList.filter(_._1.startsWith(letter)).sortBy(_._1).map(_._2))

  def booleansToLong(booleans: List[Boolean], acc: Long = 0L, index: Int = 0): Long =
    booleans match
      case head :: next =>
        if (head) booleansToLong(booleans = next, acc = acc + (1L << index), index = index + 1)
        else booleansToLong(booleans = next, acc = acc, index = index + 1)
      case Nil => acc

  def longToBooleans(long: Long, acc: List[Boolean] = List.empty, index: Int = 0): List[Boolean] =
    if (long == 0 && index > 0) acc
    else if ((long & (1L << index)) != 0) longToBooleans(long - (1L << index), true +: acc, index + 1)
    else longToBooleans(long, false +: acc, index + 1)

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

  case class Swap(left: Door, right: Door) {
    def swap: (Door, Door) = (left.copy(result = right.result), right.copy(result = left.result))
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
  }
}
