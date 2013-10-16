package ticTacToe.ai

import ticTacToe.Board
import ticTacToe.ai.rule.AiRule

trait ComputerPlayer {

  def takeSquare(implicit board: Board): Board

  def calcNextMove(rules: Seq[AiRule])(implicit board: Board): Option[(Int, Int)] = {
    for (rule <- rules) {
      rule.squareToPlay(board) match {
        case None =>
        case Some(square: (Int, Int)) => return Some(square)
      }
    }
    return None
  }

}