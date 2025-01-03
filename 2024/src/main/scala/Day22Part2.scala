import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day22Part2 {
  val memoization: mutable.Map[(Int, Int, Int, Int), Array[Int]] = mutable.Map.empty

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day22.txt"))(_.getLines().toList).get
    val monkeysAmount = input.size

    val sequences = input.map(_.toInt).map(sequenceN(_, 2000))

    for ((sequence, index) <- sequences.zipWithIndex) {
      for (window <- sequence.sliding(5)) {
        window match
          case a :: b :: c :: d :: e :: Nil =>
            val diffA = b - a
            val diffB = c - b
            val diffC = d - c
            val diffD = e - d

            memoization.get((diffA, diffB, diffC, diffD)) match
              case Some(array) if array(index) == 0 =>
                memoization((diffA, diffB, diffC, diffD)).update(index, e)
              case Some(array) =>
                ()
              case None =>
                memoization((diffA, diffB, diffC, diffD)) = Array.fill(monkeysAmount)(0)
                memoization((diffA, diffB, diffC, diffD)).update(index, e)
          case _ => throw new Exception("Should not happen.")
      }
    }

    val answer = memoization.view.mapValues(_.sum).toList.sortBy(_._2)(Ordering.Int.reverse).head._2

    print(answer)
  }

  @tailrec
  def sequenceN(secret: Long, count: Int, acc: List[Int] = List.empty): List[Int] =
    if (count == 0) acc
    else sequenceN(sequence(secret), count - 1, acc :+ (secret % 10).toInt)

  def sequence(secret: Long): Long = {
    def step1(secret: Long): Long = prune(mix(secret * 64, secret))

    def step2(secret: Long): Long = prune(mix(Math.floor(secret / 32.toDouble).toLong, secret))

    def step3(secret: Long): Long = prune(mix(secret * 2048, secret))

    step3(step2(step1(secret)))
  }

  def mix(value: Long, secret: Long): Long = value ^ secret

  def prune(secret: Long): Long = secret % 16777216
}
