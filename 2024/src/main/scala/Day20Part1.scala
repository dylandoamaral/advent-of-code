import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day20Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day20.txt"))(_.getLines().toList).get

    val map = input.map(_.toArray).toArray.transpose
    val size = map.length
    val graph = buildGraph(size, map)

    val startPosition =
      map.head.indices
        .flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, 'S') => (x, y) }
        .getOrElse(throw new Exception("Can't find start"))
    val endPosition =
      map.head.indices
        .flatMap(x => map.indices.map(y => (x, y, map(x)(y))))
        .collectFirst { case (x, y, 'E') => (x, y) }
        .getOrElse(throw new Exception("Can't find end"))

    val time = dijkstra(graph, startPosition)(endPosition)
    val minSavedTime = 100

    val cheats = findCheats(size, map)
    val answer = cheats
      .map(cheat => applyCheat(cheat, graph, size, map))
      .map(cheatedGraph => dijkstra(cheatedGraph, startPosition))
      .map(weights => weights.get(endPosition))
      .collect { case Some(cheatedTime) => time - cheatedTime}
      .count(_ >= minSavedTime)

    print(answer)
  }

  def findCheats(size: Int, map: Array[Array[Char]]): List[(Int, Int)] = {
    (1 until size - 1)
      .flatMap { x => (1 until size - 1)
        .map { y => (x, y) } }
      .filter((x, y) => map(x)(y) == '#' && List(map(x - 1)(y) != '#', map(x + 1)(y) != '#', map(x)(y - 1) != '#', map(x)(y + 1) != '#').count(_ == true) >= 2)
      .toList
  }

  def applyCheat(cheat: (Int, Int), graph: Map[(Int, Int), List[(Int, Int)]], size: Int, map: Array[Array[Char]]): Map[(Int, Int), List[(Int, Int)]] = {
    val (x1, y1) = cheat

    val updatedGraph = graph.map { case ((x, y), neighbours) =>
      if ((x - 1 == x1 && y == y1) || (x + 1 == x1 && y == y1) || (x == x1 && y - 1 == y1) || (x == x1 && y + 1 == y1)) (x, y) -> (neighbours :+ (x1, y1))
      else (x, y) -> neighbours
    }

    val cheatNeighbours =
      List(
        if (x1 - 1 >= 0 && List('S', '.', 'E').contains(map(x1 - 1)(y1))) Some((x1 - 1, y1)) else None,
        if (x1 + 1 < size && List('S', '.', 'E').contains(map(x1 + 1)(y1))) Some((x1 + 1, y1)) else None,
        if (y1 - 1 >= 0 && List('S', '.', 'E').contains(map(x1)(y1 - 1))) Some((x1, y1 - 1)) else None,
        if (y1 + 1 < size && List('S', '.', 'E').contains(map(x1)(y1 + 1))) Some((x1, y1 + 1)) else None
      ).collect { case Some(v) => v }

    updatedGraph + (cheat -> cheatNeighbours)
  }

  def buildGraph(size: Int, map: Array[Array[Char]]): Map[(Int, Int), List[(Int, Int)]] = {
    (0 until size).flatMap { x => (0 until size).map { y => (x, y) } }
      .filter((x, y) => List('S', '.', 'E').contains(map(x)(y)))
      .map {
        case (x, y) =>
          (x, y) ->
            List(
              if (x - 1 >= 0 && List('S', '.', 'E').contains(map(x - 1)(y))) Some((x - 1, y)) else None,
              if (x + 1 < size && List('S', '.', 'E').contains(map(x + 1)(y))) Some((x + 1, y)) else None,
              if (y - 1 >= 0 && List('S', '.', 'E').contains(map(x)(y - 1))) Some((x, y - 1)) else None,
              if (y + 1 < size && List('S', '.', 'E').contains(map(x)(y + 1))) Some((x, y + 1)) else None
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
