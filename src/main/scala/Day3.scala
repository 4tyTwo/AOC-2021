import scala.io.Source

object Day3 {
  val filename = "priv/day3.txt"
  val readings = Source.fromFile(filename).getLines.map(s => s.toCharArray().toList.map(c => c.toInt - 48)).toList
  val rowLength = readings(0).length
  val mostCommon = mostCommonBits(readings)

  def solve1() = {
    val gamma = listToInt(mostCommon)
    val epsilon = Math.pow(2, readings(0).length).toInt - 1 - gamma
    gamma * epsilon
  }

  def solve2() = {
    val (oxygenList :: List(), _) = Range(0, rowLength).foldLeft((readings, mostCommon))(makeFoldFun((a, b) => a == b))
    val (co2List :: List(), _) = Range(0, rowLength).foldLeft((readings, mostCommon))(makeFoldFun((a, b) => a != b))
    val oxygen = listToInt(oxygenList)
    val co2 = listToInt(co2List)
    oxygen * co2
  }

  private def listToInt(list: List[Int]): Int = Integer.parseInt(list.map(i => i.toString).mkString(""), 2)

  private def mostCommonBits(list: List[List[Int]]): List[Int] = {
    val length = list.length
    val rowLength = list(0).length
    val threshold = length / 2.0
    list.foldLeft(List.fill(rowLength)(0))(sumLists).map(i => if (i >= threshold) 1 else 0)
  }

  private def sumLists(acc: List[Int], curr: List[Int]): List[Int] = (acc, curr).zipped.map(_ + _)

  def makeFoldFun(cmp: ((Int, Int) => Boolean)): (((List[List[Int]], List[Int]), Int) => (List[List[Int]], List[Int])) = {
    (input: (List[List[Int]], List[Int]), index: Int) => {
      val (rs, mostCommonBits0) = input
      if (rs.length == 1) {
        (rs, mostCommonBits0)
      } else {
        val filtered = rs.filter(reading => cmp(reading(index), mostCommonBits0(index)))
        (filtered, mostCommonBits(filtered))
      }
    }
  }
}
