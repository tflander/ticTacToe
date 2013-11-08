package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.AiRule

class HumanizedAi(icon: CellState, openingRule: Option[AiRule], primaryRules: Seq[AiRule], exceptionRules: Seq[AiRule]) extends ComputerPlayer {
  require(!primaryRules.isEmpty, "Primary Ai Rules are required.  Found an empty list")

  override def takeSquare(implicit board: Board): Board = {
    require(!board.gameOver)

    def move: (Int, Int) = {

      // play opening rule if first or second move
      if (board.turnsPlayed < 2 && openingRule != None) {
        openingRule.get.squareToPlay(board) match {
          case Some(move: (Int, Int)) => return move
          case None =>
        }
      }

      // play exception rule if it applies
      for (rule <- exceptionRules) {
        rule.squareToPlay(board) match {
          case Some(move: (Int, Int)) => return move
          case None =>
        }
      }

      // play primary rules
      for (rule <- primaryRules) {
        rule.squareToPlay(board) match {
          case Some(move: (Int, Int)) => return move
          case None =>
        }
      }

      // play random
      return randomEmptySquare
    }

    return board.setCellState(move, icon)

  }
}