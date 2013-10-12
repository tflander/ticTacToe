package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Winner(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def canWinThisTurn(cells: Seq[CellState]): Boolean = {
      val iHaveAllButOne = board.boardSizeMinusOne == cells.count(_ == icon)
      val oneIsClear = 1 == cells.count(_ == Clear)
      return iHaveAllButOne && oneIsClear
    }

    def winningCell(cells: Seq[CellState]): Int = {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (cells(i) == Clear) return i
      }
      throw new IllegalArgumentException("no clear cell available")
    }

    for (r <- 0 to board.boardSizeMinusOne) {
      val cells = board.row(r)
      if (canWinThisTurn(cells)) {
        return Some((winningCell(cells), r))
      }
    }

    for (c <- 0 to board.boardSizeMinusOne) {
      val cells = board.col(c)
      if (canWinThisTurn(cells)) {
        return Some(c, (winningCell(cells)))
      }
    }

    val cellsInDiagOne = board.diagonalOne
    if (canWinThisTurn(cellsInDiagOne)) {
      val i = winningCell(cellsInDiagOne)
      return Some((i, i))
    }

    val cellsInDiagTwo = board.diagonalTwo
    if (canWinThisTurn(cellsInDiagTwo)) {
      val i = winningCell(cellsInDiagTwo)
      return Some((i, board.boardSizeMinusOne - i))
    }
    
    return None
  }

}