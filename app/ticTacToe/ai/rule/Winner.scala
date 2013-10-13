package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Winner(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def canIWinThisTurn = canWinThisTurn(board, icon)(_)
    def winningPosition = winningPositionOnBoardForCellSequence(board)(_)
    
    for (r <- 0 to board.boardSizeMinusOne) {
      val cells = board.row(r)
      if (canIWinThisTurn(cells)) {
        return Some((winningPosition(cells), r))
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val cells = board.column(c)
      if (canIWinThisTurn(cells)) {
        return Some(c, (winningPosition(cells)))
      }
    }

    val cellsInDiagOne = board.diagonalOne
    if (canIWinThisTurn(cellsInDiagOne)) {
      val i = winningPosition(cellsInDiagOne)
      return Some((i, i))
    }

    val cellsInDiagTwo = board.diagonalTwo
    if (canIWinThisTurn(cellsInDiagTwo)) {
      val i = winningPosition(cellsInDiagTwo)
      return Some((i, board.boardSizeMinusOne - i))
    }
    
    return None
  }

}