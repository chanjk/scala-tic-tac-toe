package tictactoe

import scala.io.StdIn.readLine
import scala.util.{ Try, Success, Failure }

import Game._

object TicTacToe extends App {
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

  val (p1Token, p2Token) = ('O', 'X')

  println(stringify(emptyBoard))
  println(s"""$p1Token to start. Enter your move as "row column" (no quotes):""")
  continue(emptyBoard, (p1Token, p2Token))
}
