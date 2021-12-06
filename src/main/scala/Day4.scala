import scala.io.Source

object Day4 {
  val filename = "priv/day4.txt"
  val readings = Source.fromFile(filename).getLines.toList

  val rawDraws :: rawBoards = readings
  val draws = rawDraws.split(",").map(s => s.toInt).toList
  val boards = rawBoards.filter(s => s != "").grouped(5).map(readBoard).toList

  def solve1(): Int = {
    val filter = (board: Board) => true
    val update = (prev: Int, curr: Int) => if (prev != -1) prev else curr
    draws.foldLeft((-1, boards))(makeFoldFun(filter, update))._1
  }

  def solve2(): Int = {
    val filter = (board: Board) => !board.isBingo
    val update = (prev: Int, curr: Int) => curr
    draws.foldLeft((0, boards))(makeFoldFun(filter, update))._1
  }

  def makeFoldFun(filter: (Board => Boolean), update: (Int, Int) => Int) = {
    (acc: (Int, List[Board]), draw: Int) => {
      val (lastBingoBoardScore, boardsLeft0) = acc
      val boardsLeft = boardsLeft0.map(_.mark(draw))
      boardsLeft.find(_.isBingo()) match {
      case None => (lastBingoBoardScore, boardsLeft)
        case Some(bingo) => {
          val boardScore = bingo.unmarkedBoardSum * draw
          (update(lastBingoBoardScore, boardScore), boardsLeft.filter(filter))
        }
      }
    }
  }

  private def readBoard(board: List[String]): Board =
    Board(board.map(s => s.split(" ").filter(s1 => s1 != "").toList.map(s2 => s2.toInt)))
}

object Board {
  def apply(board: List[List[Int]]): Board = {
    var unmarkedBoardSum = board.foldLeft(0)((sum, list) => sum + list.sum)
    val rows = (0 to board.length - 1).map(v => (v, 0)).toMap
    val cols = (0 to rows.size - 1).map(v => (v, 0)).toMap
    Board(board, rows, cols, unmarkedBoardSum)
  }
}

case class Board(val board: List[List[Int]], val rows: Map[Int, Int], val cols: Map[Int, Int], val unmarkedBoardSum: Int) {
  def mark(value: Int): Board = {
    getPos(value) match {
      case Some((row, col)) => {
        Board(
          board,
          rows + (row -> (rows(row) + 1)),
          cols + (col -> (cols(col) + 1)),
          unmarkedBoardSum - value
        )
      }
      case None => Board(board, rows, cols, unmarkedBoardSum)
    }
  }

  def isBingo(): Boolean = {
    rows.exists(kv => kv._2 == board.length) || cols.exists(kv => kv._2 == board(0).length)
  }

  private def getPos(value: Int): Option[(Int, Int)] = {
    var pos: Option[(Int, Int)] = None
    for (i <- 0 to board.length - 1) {
      for (j <- 0 to board(0).length - 1) {
        if (board(i)(j) == value) pos = Some((i, j))
      }
    }
    pos
  }
}
