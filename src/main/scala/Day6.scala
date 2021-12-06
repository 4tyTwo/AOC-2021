import scala.io.Source

object Day6 {
  val filename = "priv/day6.txt"
  val school = Source.fromFile(filename).getLines.toList(0).split(",").map(Integer.parseInt).toList
  var cache = Map[(Int, Int), Long]()

  def solve1(): Long = {
    simulate(school, 80)
  }

  def solve2(): Long = {
    simulate(school, 256)
  }

  def simulate(school: List[Int], days: Int): Long = {
    school.map(simulateFish(_, days)).sum
  }

  def simulateFish(timer: Int, days: Int): Long = {
    cache get (timer, days) match {
      case None => {
        val daysLeftToBreed = timer + 1
        if (days >= daysLeftToBreed) {
          val r = simulateFish(6, days - daysLeftToBreed) + simulateFish(8, days - daysLeftToBreed)
          cache = cache + ((timer, days) -> r)
          r
        } else 1
      }
      case Some(value) => 
        value
    }
  }
}
