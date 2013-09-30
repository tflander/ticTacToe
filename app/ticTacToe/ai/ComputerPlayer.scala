package ticTacToe.ai

import ticTacToe.Board

trait ComputerPlayer {
  
  def takeSquare(board: Board): Board

}