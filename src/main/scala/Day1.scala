import scala.io.Source

object Day1 {

  val filename = "priv/day1.txt"
  val meausurements = Source.fromFile(filename).getLines.map(s => s.toInt).toList

  def solve1(): Int = {
    val first = meausurements(0)
    val rest = meausurements.tail
    val (res, _) = rest.foldLeft((0, first))(folder)
    res
  }

  def solve2(): Int = {
    val a :: b :: c :: _ = meausurements.take(3)
    val rest = meausurements.takeRight(meausurements.length - 3)
    val (res, _) = rest.foldLeft(0, (a, b, c))(folderGroup)
    res
  }

  private def folder(input: (Int, Int), curr: Int): (Int, Int) = {
    val (acc0, prev) = input
    val acc = if (curr > prev) acc0 + 1 else acc0
    (acc, curr)
  }

  private def folderGroup(input: (Int, (Int, Int, Int)), curr: Int): (Int, (Int, Int, Int)) = {
    val (acc0, (a, b, c)) = input
    val lastGroupSum = a + b + c
    val currGroupSum = b + c + curr
    val acc = if (currGroupSum > lastGroupSum) acc0 + 1 else acc0
    (acc, (b, c , curr))
  }
}
