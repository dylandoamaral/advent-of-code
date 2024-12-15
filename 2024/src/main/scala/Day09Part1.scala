import scala.io.Source
import scala.util.Using

object Day09Part1 {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromResource("Day09.txt"))(_.getLines().toList).get
    val diskMap = input.head.map(_.toString.toInt).toList
    val fileIds =
      diskMap
        .zipWithIndex.filter(_._2 % 2 == 0)
        .flatMap { case (size, id) => (0 until size).map(_ => id / 2) }

    val answer = compressDiskMap(diskMap, fileIds).zipWithIndex.map { case (id, position) => id * position }.map(_.toLong).sum

    print(answer)
  }

  def compressDiskMap(
                       diskMap: List[Int],
                       fileIds: List[Int],
                       acc: List[Int] = List.empty,
                       isFreeSpace: Boolean = false): List[Int] = {
    diskMap match
      case head :: tail if isFreeSpace =>
        val (unusedFileIds, usedFileIds) = fileIds.splitAt(fileIds.length - head)
        compressDiskMap(
          diskMap = tail,
          fileIds = unusedFileIds,
          acc = acc ++ usedFileIds.reverse
        )
      case head :: tail =>
        val (usedFileIds, unusedFileIds) = fileIds.splitAt(head)

        compressDiskMap(
          diskMap = tail,
          fileIds = unusedFileIds,
          acc = acc ++ usedFileIds,
          isFreeSpace = true
        )
      case Nil => acc
  }
}
