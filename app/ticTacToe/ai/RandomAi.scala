package ticTacToe.ai

import ticTacToe.Board
import ticTacToe.CellState._
import scala.util.Random
import java.util.Date

class RandomAi(icon: CellState) extends ComputerPlayer {
  
  val random = new Random(new Date().getTime)
  
  override def takeSquare(implicit board: Board): Board = {
    require((!board.gameOver || board.winner != Clear) && !board.emptySquares.isEmpty)
    
    def randomEmptySquare(board: Board): (Int, Int) = {
      
      while(true) {
        val col = random.nextInt(3) 
        val row = random.nextInt(3)
        if (board.cellState(col, row) == Clear) {
          return (col, row)
        }
      }
      
      (0, 0)
    }
    
    val square = randomEmptySquare(board) 
    board.setCellState(square, icon)
  }

}