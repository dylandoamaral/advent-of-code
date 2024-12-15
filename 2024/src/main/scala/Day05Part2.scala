import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day05Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day05.txt"))(_.getLines().toList).get
    val (rawRules, rawUpdates) = input.splitAt(input.indexOf(""))

    val rules = rawRules.collect { case s"$beforePage|$afterPage" => (beforePage.toInt, afterPage.toInt) }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val updates = rawUpdates.tail.map(_.split(",").map(_.toInt).toList)

    val answer = updates
      .filter(update => !isUpdateValid(rules, update))
      .map(update => orderInvalidUpdate(rules, update))
      .map(update => update.drop(update.length / 2).head).sum

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

  def orderInvalidUpdate(rules: Map[Int, List[Int]], update: List[Int]): List[Int] = {
    val newOrder = rules.foldLeft(update) {case (acc, curr) => applyRules(curr, acc)}

    if (!isUpdateValid(rules, newOrder)) orderInvalidUpdate(rules, newOrder)
    else newOrder
  }

  def applyRules(rule: (Int, List[Int]), update: List[Int]): List[Int] = {
    update.find(page => rule._1 == page) match
      case Some(value) => rule._2.foldLeft(update) {case (acc, curr) => applyRule(value, curr, acc)}
      case None => update
  }

  def applyRule(beforePage: Int, afterPage: Int, update: List[Int]): List[Int] = {
    if (update.takeWhile(_ != beforePage).contains(afterPage)) costlySwap(update, beforePage, afterPage)
    else update
  }

  def costlySwap(list: List[Int], value1: Int, value2: Int): List[Int] = {
    list.map {
      case `value1` => value2
      case `value2` => value1
      case other => other
    }
  }
}