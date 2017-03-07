package tictactoe

import Game._
import Minimax._

object Solver {
  def winner(board: Board) =
    if (!isWin(board)) None
    else if (board.count(_ == Some('O')) > board.count(_ == Some('X'))) Some('O')
    else Some('X')

  def possibleNextMoves(board: Board) = {
    val nextToken = if (board.count(_ == Some('O')) > board.count(_ == Some('X'))) 'X' else 'O'
    (for (i <- 0 until board.length if board(i).isEmpty) yield board updated (i, Some(nextToken))).toSeq
  }

  def evaluate(board: Board) = winner(board) match {
    case Some('O') => 10
    case Some('X') => -10
    case _ => 0
  }

  def isEndState(board: Board) = isWin(board) || isFull(board)

  lazy val allGameStates = minimax(emptyBoard, possibleNextMoves, isEndState, evaluate)
}
