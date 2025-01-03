import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day22Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day22.txt"))(_.getLines().toList).get

    val answer = input.map(_.toLong).map(sequenceN(_, 2000)).sum

    print(answer)
  }

  @tailrec
  def sequenceN(secret: Long, count: Int): Long =
    if (count == 0) secret
    else sequenceN(sequence(secret), count - 1)

  def sequence(secret: Long): Long = {
    def step1(secret: Long): Long = prune(mix(secret * 64, secret))
    def step2(secret: Long): Long = prune(mix(Math.floor(secret / 32.toDouble).toLong, secret))
    def step3(secret: Long): Long = prune(mix(secret * 2048, secret))

    step3(step2(step1(secret)))
  }

  def mix(value: Long, secret: Long): Long = value ^ secret
  def prune(secret: Long): Long = secret % 16777216
}
