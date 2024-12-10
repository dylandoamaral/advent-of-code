import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day9Part2 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day9.txt"))(_.getLines().toList).get
    val diskMap = input.head.map(_.toString.toInt).toList.zipWithIndex.map({ case (size, id) =>
      if (id % 2 == 0) (size, id / 2)
      else (size, -1)
    }
    )
    val reversedFileIds = diskMap.filter(_._2 != -1).reverse

    val answer = compressDiskMap(diskMap, reversedFileIds)
      .zipWithIndex
      .filter(_._1 != -1)
      .map { case (id, position) => id * position }
      .map(_.toLong)
      .sum

    print(answer)
  }

  @tailrec
  def compressDiskMap(
                       diskMap: List[(Int, Int)],
                       reversedFileIds: List[(Int, Int)],
                       acc: List[Int] = List.empty,
                     ): List[Int] = {
    diskMap match
      // free space
      case (size, id) :: tail if !reversedFileIds.contains((size, id)) =>
        val (fulfilledSpace, newReversedFileIds) = fullFillFreeSpace(freeSpace = size, reversedFileIds = reversedFileIds)
        compressDiskMap(
          diskMap = tail,
          reversedFileIds = newReversedFileIds,
          acc = acc ++ fulfilledSpace,
        )
      case (size, id) :: tail =>
        compressDiskMap(
          diskMap = tail,
          reversedFileIds = reversedFileIds.dropRight(1),
          acc = acc ++ (0 until size).map(_ => id),
        )
      case Nil => acc
  }

  @tailrec
  def fullFillFreeSpace(
                         freeSpace: Int,
                         reversedFileIds: List[(Int, Int)],
                         accFulfilledSpace: List[Int] = List.empty,
                         accReversedFileIds: List[(Int, Int)] = List.empty
                       ): (List[Int], List[(Int, Int)]) = {
    reversedFileIds match
      case head :: tail if head._1 > freeSpace =>
        fullFillFreeSpace(
          freeSpace = freeSpace,
          reversedFileIds = tail,
          accFulfilledSpace = accFulfilledSpace,
          accReversedFileIds = accReversedFileIds :+ head
        )
      case head :: tail if head._1 == freeSpace =>
        (
          accFulfilledSpace ++ (0 until freeSpace).map(_ => head._2),
          accReversedFileIds ++ tail
        )
      case head :: tail =>
        fullFillFreeSpace(
          freeSpace = freeSpace - head._1,
          reversedFileIds = tail,
          accFulfilledSpace = accFulfilledSpace ++ (0 until head._1).map(_ => head._2),
          accReversedFileIds = accReversedFileIds
        )
      case Nil =>
        (
          accFulfilledSpace ++ (0 until freeSpace).map(_ => -1),
          accReversedFileIds
        )
  }
}
