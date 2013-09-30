package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Blocker(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X
  
  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    for (r <- 0 to board.boardSizeMinusOne) {
      val cellsInRow = for (c <- 0 to board.boardSizeMinusOne) yield board.cellState(c, r)
      if (board.boardSizeMinusOne == cellsInRow.count(_ == opponent)) {
        for (c <- 0 to board.boardSizeMinusOne) {
          if (board.cellState(c, r) == Clear) return Some((c, r))
        }
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val cellsInCol = for (r <- 0 to board.boardSizeMinusOne) yield board.cellState(c, r)
      if (board.boardSizeMinusOne == cellsInCol.count(_ == opponent)) {
        for (r <- 0 to board.boardSizeMinusOne) {
          if (board.cellState(c, r) == Clear) return Some((c, r))
        }
      }
    }

    val cellsInDiagOne = for (i <- 0 to board.boardSizeMinusOne) yield board.cellState(i, i)
    if (board.boardSizeMinusOne == cellsInDiagOne.count(_ == opponent)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (board.cellState(i, i) == Clear) return Some((i, i))
      }
    }

    val cellsInDiagTwo = for (i <- 0 to board.boardSizeMinusOne) yield board.cellState(i, board.boardSizeMinusOne - i)
    if (board.boardSizeMinusOne == cellsInDiagTwo.count(_ == opponent)) {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (board.cellState(i, board.boardSizeMinusOne - i) == Clear) return Some((i, board.boardSizeMinusOne - i))
      }
    }
    None
  }

}