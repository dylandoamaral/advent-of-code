import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day19Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day19.txt"))(_.getLines().toList).get

    val patterns = input.head.split(", ").toList
    val designs = input.drop(2)
    val memoization = mutable.Map.empty[String, Boolean]
    val answers = designs.map(design => isPossible(design, patterns, memoization)).count(_ == true)

    print(answers)
  }

  def isPossible(design: String, patterns: List[String], memoization: mutable.Map[String, Boolean], acc: String = ""): Boolean = {
    if (memoization.contains(design)) memoization(design)
    else if (design == "") true
    else {
      val answer = patterns.exists(
        pattern =>
          if (design.startsWith(pattern)) {
            memoization(acc + pattern) = true
            isPossible(design.drop(pattern.length), patterns, memoization, acc + pattern)
          } else false
      )

      if (!answer) memoization(design) = false

      answer
    }
  }
}
