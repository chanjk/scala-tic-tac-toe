package tictactoe

import scala.io.StdIn.readLine
import scala.util.{ Try, Success, Failure }

object Game {
  type Board = Vector[Option[Char]]

  def stringify(board: Board) =
    "   0   1   2\n" ++
      (for (idx <- 0 until 9 by 3) yield s"${idx / 3} ${board.slice(idx, idx + 3).map(_.getOrElse('-')).mkString(" ", " | ", " ")}\n")
      .mkString("  -----------\n")

  def parse(input: String) = input.split("\\s+").take(2).map(_.toInt) match { case Array(row, col) => (row, col) }

  def indexFromPos(row: Int, col: Int) = row * 3 + col

  def isValidPos(board: Board, row: Int, col: Int) = row >= 0 && row <= 2 && col >= 0 && col <= 2 && board(indexFromPos(row, col)).isEmpty

  def isWin(board: Board) = {
    val rows = for (idx <- 0 until 9 by 3) yield Set(idx, idx + 1, idx + 2)
    val cols = for (idx <- 0 until 3) yield Set(idx, idx + 3, idx + 6)
    val dias = Seq(Set(0, 4, 8), Set(2, 4, 6))

    (rows ++ cols ++ dias).map(_.map(board(_))) exists (s => s.size == 1 && s != Set(None))
  }

  def isFull(board: Board) = board forall (!_.isEmpty)
}
