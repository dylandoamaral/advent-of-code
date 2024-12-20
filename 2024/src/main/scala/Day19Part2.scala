import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day19Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day19.txt"))(_.getLines().toList).get

    val patterns = input.head.split(", ").toList
    val designs = input.drop(2)
    val memoization = mutable.Map.empty[String, Long]
    val answers = designs.map(possibility => countPossibility(possibility, patterns, memoization)).sum

    print(answers)
  }

  def countPossibility(design: String, patterns: List[String], memoization: mutable.Map[String, Long], acc: String = ""): Long = {
    if (memoization.contains(design)) memoization(design)
    else if (design == "") 1
    else {
      val answer = patterns.map(
        pattern =>
          if (design.startsWith(pattern)) countPossibility(design.drop(pattern.length), patterns, memoization, acc + pattern)
          else 0
      ).sum

      if (answer == 0) memoization(design) = 0
      else memoization(design) = answer

      answer
    }
  }
}
