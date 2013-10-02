package controllers

import play.api._
import play.api.mvc._
import ticTacToe.ai.SmartestAi
import ticTacToe.CellState._
import ticTacToe.Board
import controllers.support.BoardState

object Application extends Controller {
  
  
  def index = Action {
    val board = Board()
    val ai = new SmartestAi(board.nextPlayer)
    Ok(views.html.index("", ai.takeSquare(board)))
  }

  def move(setup: String) = Action {
    val cellStates = BoardState.get(setup)
    val board = Board()
    	.setCellState(0, 0, cellStates(0))
    	.setCellState(1, 0, cellStates(1))
    	.setCellState(2, 0, cellStates(2))
    	.setCellState(0, 1, cellStates(3))
    	.setCellState(1, 1, cellStates(4))
    	.setCellState(2, 1, cellStates(5))
    	.setCellState(0, 2, cellStates(6))
    	.setCellState(1, 2, cellStates(7))
    	.setCellState(2, 2, cellStates(8))
    val ai = new SmartestAi(board.nextPlayer)
    val updatedBoard = if(board.gameOver) board else ai.takeSquare(board)
    val message = updatedBoard.gameOver match {
      case false => ""
      case true => updatedBoard.winner match {
        case Clear => "Tie (Cat Game)"
        case X => "I Win!!!"
        case O => "You Win??  You must have cheated!!!"
      }
    }
    Logger.info(setup)
    if(!message.isEmpty) Logger.info(message)
    Ok(views.html.index(message, updatedBoard))
  }
  
}