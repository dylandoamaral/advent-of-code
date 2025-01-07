import scala.io.Source
import scala.util.Using

object Day25Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day25.txt"))(_.getLines().toList).get

    val (keys, locks) = input.filter(_ != "").grouped(7).partition(!_.head.contains("."))
    val keyHeights = keys.map(computeHeights).toList
    val lockHeights = locks.map(schema => computeHeights(schema.reverse)).toList

    val result =
      (
        for {
          keyHeight <- keyHeights
          lockHeight <- lockHeights
          if heightsMatch(keyHeight, lockHeight)
        } yield 1
      ).sum

    println(result)
  }

  def computeHeights(schema: List[String]): (Int, Int, Int, Int, Int) = {
    schema.foldLeft((-1, -1, -1, -1, -1)) { case (acc, curr) =>
      curr.toList match
        case h1 :: h2 :: h3 :: h4 :: h5 :: Nil =>
          (
            acc._1 + (if (h1 == '#') 1 else 0),
            acc._2 + (if (h2 == '#') 1 else 0),
            acc._3 + (if (h3 == '#') 1 else 0),
            acc._4 + (if (h4 == '#') 1 else 0),
            acc._5 + (if (h5 == '#') 1 else 0)
          )
        case _ => throw new Exception("Should not happen")
    }
  }

  def heightsMatch(lock: (Int, Int, Int, Int, Int), key: (Int, Int, Int, Int, Int)): Boolean =
    lock._1 + key._1 <= 5
      && lock._2 + key._2 <= 5
      && lock._3 + key._3 <= 5
      && lock._4 + key._4 <= 5
      && lock._5 + key._5 <= 5
}
