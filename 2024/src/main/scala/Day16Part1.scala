import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day16Part1 {
  enum Direction {
    case East
    case West
    case North
    case South
  }

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day16.txt"))(_.getLines().toList).get

    val map = input.map(_.toArray).toArray.transpose
    val start =
      map.head.indices
        .flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, 'S') => (x, y) }
        .getOrElse(throw new Exception("Can't find start"))
    val end =
      map.head.indices
        .flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, 'E') => (x, y) }
        .getOrElse(throw new Exception("Can't find end"))
    val size = map.length

    val graph =  buildGraph(map, size)
    val score = List(Direction.North, Direction.East, Direction.South, Direction.West).map {
      direction => dijkstra(graph, Node(start._1, start._2, Direction.East), Node(end._1, end._2, direction))
    }.collect { case Some(value) => value }.min

    print(score)
  }

  case class Node(x: Int, y: Int, direction: Direction)

  case class Edge(to: Node, weight: Int)

  def buildGraph(map: Array[Array[Char]], size: Int): Map[Node, List[Edge]] =
    map.head.indices.flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
      .filter { case (_, _, character) => character != '#'}
      .flatMap {
        case (x, y, _) => List((x, y, Direction.North), (x, y, Direction.East), (x, y, Direction.South), (x, y, Direction.West))
      }
      .map {
        case (x, y, direction) =>
          Node(x, y, direction) -> List(
            if (y - 1 >= 0 && List('.', 'E').contains(map(x)(y - 1))) {
              if (direction == Direction.North) Some(Edge(Node(x, y - 1, direction), 1))
              else Some(Edge(Node(x, y - 1, Direction.North), 1001))
            } else None,
            if (x + 1 < size && List('.', 'E').contains(map(x + 1)(y))) {
              if (direction == Direction.East) Some(Edge(Node(x + 1, y, direction), 1))
              else Some(Edge(Node(x + 1, y, Direction.East), 1001))
            } else None,
            if (y + 1 < size && List('.', 'E').contains(map(x)(y + 1))) {
              if (direction == Direction.South) Some(Edge(Node(x, y + 1, direction), 1))
              else Some(Edge(Node(x, y + 1, Direction.South), 1001))
            } else None,
            if (x - 1 >= 0 && List('.', 'E').contains(map(x - 1)(y))) {
              if (direction == Direction.West) Some(Edge(Node(x - 1, y, direction), 1))
              else Some(Edge(Node(x - 1, y, Direction.West), 1001))
            } else None,
          )
            .collect { case Some(v) => v }
      }.toMap

  def dijkstra(graph: Map[Node, List[Edge]], start: Node, end: Node): Option[Int] = {
    val distances = mutable.Map[Node, Int]().withDefaultValue(Int.MaxValue)
    val priorityQueue = mutable.PriorityQueue[(Node, Int)]()(Ordering.by(-_._2))
    val visited = mutable.Set[Node]()

    distances(start) = 0
    priorityQueue.enqueue((start, 0))

    while (priorityQueue.nonEmpty) {
      val (currentNode, currentDistance) = priorityQueue.dequeue()

      if (!visited(currentNode)) {
        visited += currentNode

        if (currentNode == end) return Some(currentDistance)

        for (Edge(neighbor, weight) <- graph.getOrElse(currentNode, List())) {
          val newDistance = currentDistance + weight

          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            priorityQueue.enqueue((neighbor, newDistance))
          }
        }
      }
    }

    None
  }
}
