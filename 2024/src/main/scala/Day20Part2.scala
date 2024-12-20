import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day20Part2 {
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

    val paths = dijkstra(graph, startPosition)
    val time = paths(endPosition)

    val minSavedTime = 100

    val cheats = findCheats(size, map, paths)

    var answer = 0

    for (cheat <- cheats) {
      val distanceFromStartToCheat = paths(cheat._1)
      val distanceOfCheat = Math.abs(cheat._1._1 - cheat._2._1) + Math.abs(cheat._1._2 - cheat._2._2)
      val distanceFromCheatToEnd = paths(endPosition) - paths(cheat._2)
      val distance = distanceFromStartToCheat + distanceOfCheat + distanceFromCheatToEnd

      if (time - distance  >= minSavedTime) answer += 1
    }

    print(answer)
  }

  def findCheats(size: Int, map: Array[Array[Char]], paths: Map[(Int, Int), Int]): List[((Int, Int), (Int, Int))] = {
    (1 until size - 1).flatMap { x => (1 until size - 1).map { y => (x, y) } }
      .filter((x, y) => List('S', '.').contains(map(x)(y)))
      .flatMap((x, y) => {
        val maxPicoseconds = 20
        (1 to maxPicoseconds).flatMap { picosecond =>
          (-picosecond to picosecond).flatMap { vx => (-picosecond to picosecond).map { vy => (vx, vy) } }
            .filter((vx, vy) => Math.abs(vx) + Math.abs(vy) == picosecond && picosecond > 1)
            .map((vx, vy) => (x + vx, y + vy))
            .filter((dx, dy) =>
              dx >= 1 &&
                dx < size - 1 &&
                dy >= 1 &&
                dy < size - 1 &&
                List('S', '.', 'E').contains(map(dx)(dy)) &&
                picosecond < paths(dx, dy) - paths(x, y)
            )
            .map((dx, dy) => ((x, y), (dx, dy)))
        }
      }).toList
  }

  def buildGraph(size: Int, map: Array[Array[Char]]): Map[(Int, Int), List[((Int, Int), Int)]] = {
    (0 until size).flatMap { x => (0 until size).map { y => (x, y) } }
      .filter((x, y) => List('S', '.').contains(map(x)(y)))
      .map {
        case (x, y) =>
          (x, y) ->
            List(
              if (x - 1 >= 0 && List('S', '.', 'E').contains(map(x - 1)(y))) Some((x - 1, y)) else None,
              if (x + 1 < size && List('S', '.', 'E').contains(map(x + 1)(y))) Some((x + 1, y)) else None,
              if (y - 1 >= 0 && List('S', '.', 'E').contains(map(x)(y - 1))) Some((x, y - 1)) else None,
              if (y + 1 < size && List('S', '.', 'E').contains(map(x)(y + 1))) Some((x, y + 1)) else None
            ).collect { case Some(v) => (v, 1) }
      }
      .toMap
  }

  def dijkstra[A](graph: Map[A, List[(A, Int)]], source: A): Map[A, Int] = {
    val distances = mutable.Map[A, Int]().withDefaultValue(Int.MaxValue)
    distances(source) = 0

    val priorityQueue = mutable.PriorityQueue[(A, Int)]()(Ordering.by(-_._2)) // Min-Heap
    priorityQueue.enqueue((source, 0))

    while (priorityQueue.nonEmpty) {
      val (currentNode, currentDistance) = priorityQueue.dequeue()

      if (currentDistance <= distances(currentNode)) {
        for ((neighbor, weight) <- graph.getOrElse(currentNode, List())) {
          val newDistance = currentDistance + weight
          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            priorityQueue.enqueue((neighbor, newDistance))
          }
        }
      }
    }

    distances.toMap
  }
}
