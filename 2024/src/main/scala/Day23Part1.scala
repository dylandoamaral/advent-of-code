import scala.io.Source
import scala.util.Using

object Day23Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day23.txt"))(_.getLines().toList).get

    val connections = input.collect { case s"$left-$right" => (left, right) }

    val mapping = (connections ++ connections.map(_.swap)).groupMap(_._1)(_._2)

    val answer = countInterConnectedComputers(mapping) / 2

    print(answer)
  }

  def countInterConnectedComputers(mapping: Map[String, List[String]], result: Int = 0): Int =
    mapping.toList match
      case (computer, linkedComputers) :: tail =>
        val addition =
          (
            for {
              linkedComputer <- linkedComputers
              proxyLinkedComputer <- mapping(linkedComputer)
              if mapping(proxyLinkedComputer).contains(computer)
              if computer.startsWith("t") || linkedComputer.startsWith("t") || proxyLinkedComputer.startsWith("t")
            } yield 1
          ).sum

        countInterConnectedComputers(
          mapping.removed(computer).view.mapValues(_.filter(_ != computer)).toMap,
          result + addition
        )
      case Nil => result
}
