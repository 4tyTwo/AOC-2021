import scala.io.Source
import scala.math.Ordering.Implicits._

case class Point2D(val x: Int, val y: Int) extends Ordered[Point2D] {
  def compare(that: Point2D): Int = implicitly[Ordering[Tuple2[Int, Int]]].compare((this.x, this.y), (that.x, that.y))
}

object Day5 {
  val filename = "priv/day5.txt"
  val input = Source.fromFile(filename).getLines.toList
  val points = input.map(parseInputLine)

  def solve1() = {
    solveWith(pointsToStraightLine)
  }

  def solve2() = {
    solveWith(pointsToLine)
  }

  private def solveWith(pointsFun: (Point2D, Point2D) => List[Point2D]): Int = {
    val heatMap = countPoints(points.map(pair => pointsFun(pair._1, pair._2)).flatten)
    calculateScore(heatMap)
  }

  private def calculateScore(heatMap: Map[Point2D, Int]) = 
    heatMap.foldLeft(0)((acc, kv) => if(kv._2 >= 2) acc + 1 else acc)

  private def countPoints(points: List[Point2D]): Map[Point2D, Int] = {
    points.foldLeft(Map[Point2D, Int]())((acc, point) => acc + (point -> (acc.getOrElse(point, 0) + 1)))
  }

  private def pointsToStraightLine(a: Point2D, b: Point2D): List[Point2D] = {
    if (a.x == b.x || a.y == b.y) {
      val xRange = (Math.min(a.x, b.x) to Math.max(a.x, b.x)).map(Point2D(_, a.y)).toList
      val yRange = (Math.min(a.y, b.y) to Math.max(a.y, b.y)).map(Point2D(a.x, _)).toList
      (xRange ::: yRange).toSet.toList
    } else List()
  }

  private def pointsToLine(a: Point2D, b: Point2D): List[Point2D] = {
    if (a.x != b.x && a.y != b.y) {
      val signX = if (a.x < b.x) 1 else -1
      val signY = if (a.y < b.y) 1 else -1
      var points: List[Point2D] = List()
      for (i <- 0 to Math.abs(b.x - a.x)) {
        points = Point2D(a.x + (signX * i), a.y + (signY * i)) :: points
      }
      points
    } else {
      pointsToStraightLine(a, b)
    }
  }

  private def parseInputLine(line: String) = {
    val p1 :: p2 :: _ = line.split(" -> ").toList
      .map(_.split(",").toList.map(Integer.parseInt).grouped(2).toList)
      .flatten.map(l => Point2D(l(0), l(1)))
    (p1, p2)
  }
}
