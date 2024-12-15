import scala.io.Source
import scala.util.Using

object Day02Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day02.txt"))(_.getLines().toList).get

    val answer = input.map(_.split(" ").map(_.toInt)).filter(isSafe).size

    print(answer)
  }

  private def isSafe(report: Seq[Int]): Boolean = {
    val isIncreasing = report == report.sorted
    val isDecreasing = report == report.sorted(Ordering[Int].reverse)
    val isDifferingSlightly = report.sliding(2).map(_.toList).forall {
      case a :: b :: Nil => (b - a).abs >= 1 && (b - a).abs <= 3
      case _ => true
    }

    (isIncreasing || isDecreasing) && isDifferingSlightly
  }
}
