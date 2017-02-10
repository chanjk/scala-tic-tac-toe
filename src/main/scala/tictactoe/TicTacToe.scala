package tictactoe

import scala.io.StdIn.readLine
import scala.util.{ Try, Success, Failure }

object TicTacToe {
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

  def main(args: Array[String]): Unit = {
    def continue(board: Board, tokens: (Char, Char)): Unit = {
      val (curToken, nextToken) = tokens

      Try(parse(readLine("> "))).filter { case (r, c) => isValidPos(board, r, c) } match {
        case Success((row, col)) => {
          val updatedBoard = board updated (indexFromPos(row, col), Some(curToken))

          println(stringify(updatedBoard))

          if (isWin(updatedBoard)) println(s"'$curToken' wins!")
          else if (isFull(updatedBoard)) println("It's a draw!")
          else continue(updatedBoard, tokens.swap)
        }
        case Failure(_) => {
          println("Invalid position. Please try again:")
          continue(board, tokens)
        }
      }
    }

    val emptyBoard = Vector.fill(9)(None)
    val (p1Token, p2Token) = ('O', 'X')

    println(stringify(emptyBoard))
    println(s"""$p1Token to start. Enter your move as "row column" (no quotes):""")
    continue(emptyBoard, (p1Token, p2Token))
  }
}