import scala.io.Source
import scala.util.Using

object Day23Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day23.txt"))(_.getLines().toList).get

    val connections = input.collect { case s"$left-$right" => (left, right) }

    val mapping = (connections ++ connections.map(_.swap)).groupMap(_._1)(_._2)

    val answer = findLargestChain(mapping).toList.sorted.mkString(",")

    print(answer)
  }

  def findLargestChain(mapping: Map[String, List[String]]): Set[String] =
    mapping.keys
      .foldLeft((Set.empty[String], mapping)) { case ((result, accMapping), computer) =>
        val largestChain = findLargestChainFor(computer, mapping)
        val mappingWithoutComputer = mapping.removed(computer).view.mapValues(_.filter(_ != computer)).toMap

        if (largestChain.size > result.size) (largestChain, mappingWithoutComputer)
        else (result, mappingWithoutComputer)
      }
      ._1

  def findLargestChainFor(
      computer: String,
      mapping: Map[String, List[String]],
      acc: Set[String] = Set.empty,
      res: Set[String] = Set.empty
  ): Set[String] = {
    if (acc.isEmpty)
      mapping(computer).foldLeft(Set.empty[String]) { case (acc, curr) =>
        val candidate = findLargestChainFor(curr, mapping.removed(computer), acc + computer)
        if (candidate.size > acc.size) candidate else acc
      }
    else if (mapping(computer).toSet.intersect(acc) == acc)
      mapping(computer).filter(!acc.contains(_)).foldLeft(acc) { case (acc2, curr) =>
        val candidate = findLargestChainFor(curr, mapping.removed(computer), acc2 + computer)
        if (candidate.size > acc2.size) candidate else acc2
      }
    else acc
  }
}
