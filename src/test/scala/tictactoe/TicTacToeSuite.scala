package toyrobot

import org.scalatest.FunSuite

import tictactoe.Game._

class TicTacToeSuite extends FunSuite {
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
    val emptyBoard = Vector.fill(9)(None)
    val row0Win = emptyBoard patch (0, Seq(Some('O'), Some('O'), Some('O')), 3)

    assert(!isWin(emptyBoard))
    assert(isWin(row0Win))
  }
}
