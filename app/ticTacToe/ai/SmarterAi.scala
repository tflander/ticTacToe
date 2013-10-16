package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner

class SmarterAi(icon: CellState) extends ComputerPlayer {

  val randomAi = new RandomAi(icon)
  val rules = Seq(new Winner(icon), new Blocker(icon))
  
  override def takeSquare(board: Board): Board = {
    require(!board.gameOver)
    applyRules(rules, board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)      
    }
    return randomAi.takeSquare(board)
  }
}