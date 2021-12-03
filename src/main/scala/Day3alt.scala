import scala.io.Source
import collection.mutable.ArrayBuffer

object Day3Alt {
  val filename = "priv/day3.txt"
  val readings = Source.fromFile(filename).getLines.map(s => s.toCharArray).toArray

  def solve1() = {
    val mostCommon = mostCommonBits(readings).mkString
    val gamma = Integer.parseInt(mostCommon, 2)
    val epsilon = Math.pow(2, readings(0).length).toInt - 1 - gamma
    gamma * epsilon
  }

  def solve2() = {
    var filteredOxygen = readings
    var filteredCO2 = readings
    for (i <- 0 to readings(0).length - 1) {
      var mostCommonOxygen = mostCommonBits(filteredOxygen)
      if (filteredOxygen.length != 1)
        filteredOxygen = filteredOxygen.filter(arr => arr(i) == mostCommonOxygen(i))
      var mostCommonCO2 = mostCommonBits(filteredCO2)
      if (filteredCO2.length != 1)
        filteredCO2 = filteredCO2.filter(arr => arr(i) != mostCommonCO2(i))
    }
    val oxygen = Integer.parseInt(filteredOxygen(0).mkString, 2)
    val co2 = Integer.parseInt(filteredCO2(0).mkString, 2)
    oxygen * co2
  }

  private def mostCommonBits(arr: Array[Array[Char]]): Array[Char] = {
    val rowLength = arr(0).length
    var numOfOnes = new Array[Int](rowLength)
    for (j <- 0 to rowLength - 1) {
      for (i <- 0 to length - 1) {
        numOfOnes(j) = if (arr(i)(j) == '1') numOfOnes(j) + 1 else numOfOnes(j)
      }
    }
    numOfOnes.map(i => if (i >= arr.length / 2.0) '1' else '0')
  }
}
