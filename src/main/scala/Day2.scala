import scala.io.Source

object Day2 {
  val filename = "priv/day2.txt"
  val movements = Source.fromFile(filename).getLines.map(s => parseLine(s)).toList

  def solve1() = {
    val endPosition = movements.foldLeft(Position(0, 0))((pos, movement) => movement.moveFrom(pos))
    endPosition.horiz * endPosition.depth
  }

  def solve2() = {
    val (endPosition, _) = movements.foldLeft((Position(0, 0), 0))((input, movement) => movement.adjustAimAndMove(input))
    endPosition.horiz * endPosition.depth
  }


  private def parseLine(line: String): Movement = {
    val direction :: v0 :: _ = line.split(' ').toList
    val v = v0.toInt
    direction match {
      case "forward" => Forward(v)
      case "down" => Down(v)
      case "up" => Up(v)
    }
  }
}

case class Position(horiz: Int, depth: Int)

trait Movement {
  def moveFrom(position: Position): Position
  def adjustAimAndMove(input: (Position, Int)): (Position, Int)
}

case class Up(v: Int) extends Movement {
  override def moveFrom(position: Position): Position = Position(position.horiz, position.depth - v)
  override def adjustAimAndMove(input: (Position, Int)): (Position, Int) = {
    val (pos, aim) = input
    (pos, aim - v)
  }
}

case class Forward(v: Int) extends Movement {
  override def moveFrom(position: Position): Position = Position(position.horiz + v, position.depth)
  override def adjustAimAndMove(input: (Position, Int)): (Position, Int) = {
    val (Position(horiz, depth), aim) = input
    (Position(horiz + v, depth + v * aim), aim)
  }
}

case class Down(v: Int) extends Movement {
  override def moveFrom(position: Position): Position = Position(position.horiz, position.depth + v)
  override def adjustAimAndMove(input: (Position, Int)): (Position, Int) = {
    val (pos, aim) = input
    (pos, aim + v)
  }
}
