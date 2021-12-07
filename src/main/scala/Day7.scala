import scala.io.Source


object Day7 {
  val filename = "priv/day7.txt"
  val positions = Source.fromFile(filename).getLines().toList(0).split(",").map(Integer.parseInt).toList

  def solve1(): Int = {
    solve(fuelCost1)
  }

  def solve2(): Int = {
    solve(fuelCost2)
  }

  def solve(costFun: ((Int, Int) => Int)):Int = {
    (0 until positions.length).fold(Int.MaxValue)(
      (min, pos) => {
        val curr = positions.foldLeft(0)((acc, p) => acc + costFun(p, pos))
        Math.min(curr, min)
      }
    )
  }

  def fuelCost1(from: Int, to: Int): Int =
    Math.abs(to - from)

  def fuelCost2(from: Int, to: Int): Int = {
    val distance = Math.abs(to - from)
    (distance * ((1 + distance).toFloat/2)).toInt
  }
}
