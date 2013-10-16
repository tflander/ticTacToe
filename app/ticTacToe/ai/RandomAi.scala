package ticTacToe.ai

import ticTacToe.Board
import ticTacToe.CellState._

class RandomAi(icon: CellState) extends ComputerPlayer {
  
  override def takeSquare(implicit board: Board): Board = {
    require((!board.gameOver || board.winner != Clear) && !board.emptySquares.isEmpty)
        
    board.setCellState(randomEmptySquare, icon)
  }

}