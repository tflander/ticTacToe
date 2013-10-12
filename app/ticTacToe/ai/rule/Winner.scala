package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Winner(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    
    for (r <- 0 to board.boardSizeMinusOne) {
      val cellsInRow = board.row(r)
      if (board.boardSizeMinusOne == cellsInRow.count(_ == icon)) {
        for (c <- 0 to board.boardSizeMinusOne) {
          if (cellsInRow(c) == Clear) return Some((c, r))
        }
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val cellsInCol = board.col(c)
      if (board.boardSizeMinusOne == cellsInCol.count(_ == icon)) {
        for (r <- 0 to board.boardSizeMinusOne) {
          if (cellsInCol(r) == Clear) return Some((c, r))
        }
      }
    }

    val cellsInDiagOne = board.diagonalOne
    if (board.boardSizeMinusOne == cellsInDiagOne.count(_ == icon)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (cellsInDiagOne(i) == Clear) return Some((i, i))
      }
    }

    val cellsInDiagTwo = board.diagonalTwo
    if (board.boardSizeMinusOne == cellsInDiagTwo.count(_ == icon)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (cellsInDiagTwo(i) == Clear) return Some((i, board.boardSizeMinusOne - i))
      }
    }
    None
  }

}