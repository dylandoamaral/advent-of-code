import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day05Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day05.txt"))(_.getLines().toList).get
    val (rawRules, rawUpdates) = input.splitAt(input.indexOf(""))

    val rules = rawRules.collect { case s"$beforePage|$afterPage" => (beforePage.toInt, afterPage.toInt) }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val updates = rawUpdates.tail.map(_.split(",").map(_.toInt).toList)

    val answer = updates.filter(update => isUpdateValid(rules, update)).map(update => update.drop(update.length / 2).head).sum

    print(answer)
  }

  @tailrec
  def isUpdateValid(rules: Map[Int, List[Int]], update: List[Int], acc: List[Int] = List.empty): Boolean = {
    update match
      case head :: tail => isPageValid(rules, acc, head) && isUpdateValid(rules, tail, acc :+ head)
      case Nil => true
  }

  def isPageValid(rules: Map[Int, List[Int]], previousPages: List[Int], currentPage: Int): Boolean = {
    rules.get(currentPage) match
      case Some(afterPages) => previousPages.forall(page => !afterPages.contains(page))
      case None => true
  }
}
