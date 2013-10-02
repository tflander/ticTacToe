package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.CornerNearOpponent

class SmartestAi(icon: CellState) extends ComputerPlayer {

  val priority = new Priority(icon)
  val cornerNearOpponent = new CornerNearOpponent(icon)
  val opener = new Opener(icon)
  val winner = new Winner(icon)
  val blocker = new Blocker(icon)
  val opponent = if (icon == X) O else X

  override def takeSquare(board: Board): Board = {
    require(!board.gameOver)

    opener.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)      
    }
    
    winner.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)      
    }
    
    blocker.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)
    }
    
    cornerNearOpponent.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)
    }
    
    return board.setCellState(priority.squareToPlay(board).get, icon)
  }
}