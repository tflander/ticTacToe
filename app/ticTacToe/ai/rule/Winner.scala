package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Winner(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def canIWinThisTurn = canWinThisTurn(board, icon)(_)
    def winningPosition = winningPositionOnBoardForCellSequence(board)(_)
    
    for (r <- 0 to board.boardSizeMinusOne) {
      val line = board.row(r)
      if (canIWinThisTurn(line)) {
        return Some((winningPosition(line), r))
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val line = board.column(c)
      if (canIWinThisTurn(line)) {
        return Some(c, (winningPosition(line)))
      }
    }

    val diagOneLine = board.diagonalOne
    if (canIWinThisTurn(diagOneLine)) {
      val i = winningPosition(diagOneLine)
      return Some((i, i))
    }

    val diagTwoLine = board.diagonalTwo
    if (canIWinThisTurn(diagTwoLine)) {
      val i = winningPosition(diagTwoLine)
      return Some((i, board.boardSizeMinusOne - i))
    }
    
    return None
  }

}