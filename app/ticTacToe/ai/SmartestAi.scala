package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener

class SmartestAi(icon: CellState) extends ComputerPlayer {

  val randomAi = new RandomAi(icon)
  val opener = new Opener(icon)
  val winner = new Winner(icon)
  val blocker = new Blocker(icon)
  val opponent = if (icon == X) O else X

  override def takeSquare(board: Board): Board = {
    require(!board.gameOver)

    opener.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square._1, square._2, icon)      
    }
    
    winner.squareToPlay(board) match {
      case None => 
      case Some(square: (Int, Int)) => return board.setCellState(square._1, square._2, icon)      
    }
    
    blocker.squareToPlay(board) match {
      case None => return randomAi.takeSquare(board)
      case Some(square: (Int, Int)) => return board.setCellState(square._1, square._2, icon)
    }
    board
  }
}