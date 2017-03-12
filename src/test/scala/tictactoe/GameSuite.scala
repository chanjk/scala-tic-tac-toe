package toyrobot

import org.scalatest.FunSuite

import tictactoe.Game._

class GameSuite extends FunSuite {
  test("parse should convert coordinates to an integer pair") {
    assert(parse("1 2") === (1, 2))
    assert(parse("0   0") === (0, 0))
  }

  test("indexFromPos, given a row and column, should return the correct board index") {
    assert(indexFromPos(0, 0) === 0)
    assert(indexFromPos(2, 1) === 7)
  }

  test("isValidPos should return true if coordinates are within the board and refer to an available position, and false otherwise") {
    val board = Vector('O', 'X', '-',
                       '-', 'O', 'O',
                       'X', '-', '-').map(Option(_).filter(_ != '-'))
    assert(isValidPos(board, 0, 2))
    assert(!isValidPos(board, 0, 0))
    assert(!isValidPos(board, 3, 0))
  }

  test("isWin should detect wins for row, column and diagonal") {
    val row0Win = emptyBoard patch (0, Seq(Some('O'), Some('O'), Some('O')), 3)
    val row1Win = emptyBoard patch (3, Seq(Some('O'), Some('O'), Some('O')), 3)
    val row2Win = emptyBoard patch (6, Seq(Some('O'), Some('O'), Some('O')), 3)
    val col0Win = emptyBoard.updated(0, Some('O')).updated(3, Some('O')).updated(6, Some('O'))
    val col1Win = emptyBoard.updated(1, Some('O')).updated(4, Some('O')).updated(7, Some('O'))
    val col2Win = emptyBoard.updated(2, Some('O')).updated(5, Some('O')).updated(8, Some('O'))
    val dia0Win = emptyBoard.updated(0, Some('O')).updated(4, Some('O')).updated(8, Some('O'))
    val dia1Win = emptyBoard.updated(2, Some('O')).updated(4, Some('O')).updated(6, Some('O'))

    val notWon0 = emptyBoard patch (0, Seq(Some('O'), Some('X'), Some('O')), 3)
    val notWon1 = emptyBoard.updated(0, Some('X')).updated(3, Some('O')).updated(6, Some('O'))
    val notWon2 = emptyBoard.updated(0, Some('O')).updated(4, Some('O')).updated(8, Some('X'))

    assert(isWin(row0Win))
    assert(isWin(row1Win))
    assert(isWin(row2Win))
    assert(isWin(col0Win))
    assert(isWin(col1Win))
    assert(isWin(col2Win))
    assert(isWin(dia0Win))
    assert(isWin(dia1Win))

    assert(!isWin(emptyBoard))
    assert(!isWin(notWon0))
    assert(!isWin(notWon1))
    assert(!isWin(notWon2))
  }

  test("isFull should detect only full boards") {
    val notFullBoard = emptyBoard.updated(0, Some('X')).updated(4, Some('O')).updated(5, Some('O'))
    val fullBoard = emptyBoard.map(_ => Some('O'))

    assert(!isFull(emptyBoard))
    assert(!isFull(notFullBoard))
    assert(isFull(fullBoard))
  }
}
