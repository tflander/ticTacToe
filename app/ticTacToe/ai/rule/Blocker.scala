package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Blocker(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def mustIBlockThisTurn = canWinThisTurn(board, opponent)(_)
    def winningPosition = winningPositionOnBoardForCellSequence(board)(_)

    for (r <- 0 to board.boardSizeMinusOne) {
      val line = board.row(r)
      if (mustIBlockThisTurn(line)) {
        return Some((winningPosition(line), r))
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val line = board.column(c)
      if (mustIBlockThisTurn(line)) {
        return Some((c, winningPosition(line)))
      }
    }

    val diagOneLine = board.diagonalOne
    if (mustIBlockThisTurn(diagOneLine)) {
      val i = winningPosition(diagOneLine)
      return Some((i, i))
    }

    val diagTwoLine = board.diagonalTwo
    if (mustIBlockThisTurn(diagTwoLine)) {
      val i = winningPosition(diagTwoLine)
      return Some((i, board.boardSizeMinusOne - i))
    }

    return None
  }

}