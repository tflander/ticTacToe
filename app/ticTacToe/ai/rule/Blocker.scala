package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Blocker(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    for (r <- 0 to board.boardSizeMinusOne) {
      val row = board.row(r)
      if (board.boardSizeMinusOne == row.count(_ == opponent)) {
        for (c <- 0 to board.boardSizeMinusOne) {
          if (row(c) == Clear) return Some((c, r))
        }
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val col = board.col(c)
      if (board.boardSizeMinusOne == col.count(_ == opponent)) {
        for (r <- 0 to board.boardSizeMinusOne) {
          if (col(r) == Clear) return Some((c, r))
        }
      }
    }

    if (board.boardSizeMinusOne == board.diagonalOne.count(_ == opponent)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (board.diagonalOne(i) == Clear) return Some((i, i))
      }
    }

    if (board.boardSizeMinusOne == board.diagonalTwo.count(_ == opponent)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (board.diagonalTwo(i) == Clear) return Some((i, board.boardSizeMinusOne - i))
      }
    }
    None
  }

}