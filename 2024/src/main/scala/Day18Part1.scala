import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day18Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day18.txt"))(_.getLines().toList).get

    val size = 71
    val steps = 1024
    val positions = input.collect { case s"$x,$y" => (x.toInt, y.toInt) }.take(steps)
    val graph = buildGraph(size, positions)
    val answer = dijkstra(graph, (0, 0))(size - 1, size - 1)

    print(answer)
  }

  def buildGraph(size: Int, positions: List[(Int, Int)]): Map[(Int, Int), List[(Int, Int)]] = {
    (0 until size).flatMap { x => (0 until size).map { y => (x, y) } }
      .filter(!positions.contains(_))
      .map {
        case (x, y) =>
          (x, y) ->
            List(
              if (x - 1 >= 0 && !positions.contains((x - 1, y))) Some((x - 1, y)) else None,
              if (x + 1 < size && !positions.contains((x + 1, y))) Some((x + 1, y)) else None,
              if (y - 1 >= 0 && !positions.contains((x, y - 1))) Some((x, y - 1)) else None,
              if (y + 1 < size && !positions.contains((x, y + 1))) Some((x, y + 1)) else None
            ).collect { case Some(v) => v }
      }
      .toMap
  }

  def dijkstra[A](graph: Map[A, List[A]], source: A): Map[A, Int] = {
    val distances = mutable.Map[A, Int]().withDefaultValue(Int.MaxValue)
    distances(source) = 0

    val queue = mutable.Queue[A]()
    queue.enqueue(source)

    while (queue.nonEmpty) {
      val currentNode = queue.dequeue()

      for (neighbor <- graph.getOrElse(currentNode, List())) {
        if (distances(neighbor) == Int.MaxValue) {
          distances(neighbor) = distances(currentNode) + 1
          queue.enqueue(neighbor)
        }
      }
    }

    distances.toMap
  }
}
