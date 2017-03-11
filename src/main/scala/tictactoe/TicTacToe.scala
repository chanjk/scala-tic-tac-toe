package tictactoe

import scala.io.StdIn.readLine
import scala.util.{ Try, Success, Failure }

import Game._
import Minimax._
import Solver._

object TicTacToe extends App {
  def continue(board: Board, tokens: (Char, Char), gameStates: Option[Tree[Board, Int]]): Unit = {
    val (curToken, nextToken) = tokens

    val (updatedBoard, updatedGameStates) = solverToken match {
      case Some(t) if t == curToken => (gameStates.get.bestNext.state, Some(gameStates.get.bestNext))
      case _ => {
        Try(parse(readLine("> "))).filter { case (r, c) => isValidPos(board, r, c) } match {
          case Success((row, col)) => {
            val updatedBoard = board updated (indexFromPos(row, col), Some(curToken))
            val updatedGameStates = gameStates match {
              case Some(_) => gameStates.get.search(updatedBoard)
              case _ => gameStates
            }

            (updatedBoard, updatedGameStates)
          }
          case Failure(_) => {
            println("Invalid position. Please try again:")
            return continue(board, tokens, gameStates)
          }
        }
      }
    }

    println(stringify(updatedBoard))

    if (isWin(updatedBoard)) println(s"'$curToken' wins!")
    else if (isFull(updatedBoard)) println("It's a draw!")
    else continue(updatedBoard, tokens.swap, updatedGameStates)
  }

  val (p1Token, p2Token) = ('O', 'X')

  println(stringify(emptyBoard))
  println("Do you want to play against the computer (might take a while to set up)? (y/n)")

  val (gameStates, solverToken) = if (readLine.toLowerCase != "y") (None, None) else {
    println("\nDo you want to start? (y/n)")

    if (readLine.toLowerCase == "y") (Some(allGameStates), Some(p2Token))
    else (Some(allGameStates), Some(p1Token))
  }

  println(s"""\n$p1Token to start. On your turn, enter your move as "row column" (no quotes).""")
  continue(emptyBoard, (p1Token, p2Token), gameStates)
}
