import scala.io.Source
import scala.util.Using

object Day2Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day2.txt"))(_.getLines().toList).get

    val answer = input.map(_.split(" ").map(_.toInt)).count(isSafe(_))

    print(answer)
  }

  private def isSafe(report: Seq[Int], isTruncated: Boolean = false): Boolean = {
    val isIncreasing = report == report.sorted
    val isDecreasing = report == report.sorted(Ordering[Int].reverse)
    val isDifferingSlightly = report.sliding(2).map(_.toList).forall {
      case a :: b :: Nil => (b - a).abs >= 1 && (b - a).abs <= 3
      case _ => true
    }

    val isOk = (isIncreasing || isDecreasing) && isDifferingSlightly

    if (isOk) return true
    if (isTruncated) return false

    report.indices.exists { index =>
      val newReport = report.take(index) ++ report.drop(index + 1)
      isSafe(newReport, isTruncated = true)
    }
  }
}
