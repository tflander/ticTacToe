package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Blocker(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def mustIBlockThisTurn = canWinThisTurn(board, opponent)(_)
    def winningPosition = winningPositionOnBoardForCellSequence(board)(_)

    for (r <- 0 to board.boardSizeMinusOne) {
      val cells = board.row(r)
      if (mustIBlockThisTurn(cells)) {
        return Some((winningPosition(cells), r))
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val cells = board.column(c)
      if (mustIBlockThisTurn(cells)) {
        return Some((c, winningPosition(cells)))
      }
    }

    val cellsInDiagOne = board.diagonalOne
    if (mustIBlockThisTurn(cellsInDiagOne)) {
      val i = winningPosition(cellsInDiagOne)
      return Some((i, i))
    }

    val cellsInDiagTwo = board.diagonalTwo
    if (mustIBlockThisTurn(cellsInDiagTwo)) {
      val i = winningPosition(cellsInDiagTwo)
      return Some((i, board.boardSizeMinusOne - i))
    }

    return None
  }

}