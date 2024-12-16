import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day16Part2 {
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

    val graph = buildGraph(map, size)
    val visited = mutable.Set.empty[(Int, Int)]
    val score = findAllCellsVisited(graph, Node(start._1, start._2, Direction.East), end, visited).get.size

    print(score)
  }

  case class Node(x: Int, y: Int, direction: Direction)

  case class Edge(to: Node, weight: Int)

  def findAllCellsVisited(graph: Map[Node, List[Edge]], start: Node, end: (Int, Int), visited: mutable.Set[(Int, Int)], bestScore: Option[Int] = None): Option[Set[(Int, Int)]] = {
    dijkstra4(graph, start, end) match
      case Some(cellsVisited, currentScore) if bestScore.forall(_ == currentScore) =>
        if (cellsVisited.forall(visited.contains)) Some(cellsVisited)
        else {
          visited ++= cellsVisited

          val otherCellsVisited = cellsVisited.map {
            case (x, y) =>
              val graphWithoutCell = graph.filter { case (node, edges) => (node.x, node.y) != (x, y) }.map { case (node, edges) => (node, edges.filter(edge => (edge.to.x, edge.to.y) != (x, y))) }
              findAllCellsVisited(graphWithoutCell, start, end, visited, bestScore = Some(currentScore))
          }.collect { case Some(otherCellsVisited) => otherCellsVisited }.foldLeft(Set.empty[(Int, Int)])(_ ++ _)

          Some(cellsVisited ++ otherCellsVisited)
        }
      case _ =>
        None
  }

  def buildGraph(map: Array[Array[Char]], size: Int): Map[Node, List[Edge]] =
    map.head.indices.flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
      .filter { case (_, _, character) => character != '#' }
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

  def dijkstra4(graph: Map[Node, List[Edge]], start: Node, end: (Int, Int)): Option[(Set[(Int, Int)], Int)] = {
    List(Direction.North, Direction.East, Direction.South, Direction.West).map {
      direction => dijkstra(graph, start, Node(end._1, end._2, direction))
    }.collect { case Some(value) => value }.sortBy(_._2).headOption
  }

  def dijkstra(graph: Map[Node, List[Edge]], start: Node, end: Node): Option[(Set[(Int, Int)], Int)] = {
    val distances = mutable.Map[Node, Int]().withDefaultValue(Int.MaxValue)
    val priorityQueue = mutable.PriorityQueue[(Node, (Set[(Int, Int)], Int))]()(Ordering.by(-_._2._2))
    val visited = mutable.Set[Node]()

    distances(start) = 0
    priorityQueue.enqueue((start, (Set((start.x, start.y)), 0)))

    while (priorityQueue.nonEmpty) {
      val (currentNode, (currentNodes, currentDistance)) = priorityQueue.dequeue()

      if (!visited(currentNode)) {
        visited += currentNode

        if (currentNode == end) return Some((currentNodes + ((end.x, end.y)), currentDistance))

        for (Edge(neighbor, weight) <- graph.getOrElse(currentNode, List())) {
          val newDistance = currentDistance + weight

          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            priorityQueue.enqueue((neighbor, (currentNodes + ((neighbor.x, neighbor.y)), newDistance)))
          }
        }
      }
    }

    None
  }
}
