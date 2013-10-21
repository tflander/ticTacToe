package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.AiRule

class HumanizedAi(icon: CellState, rulesWithOdds: Seq[(AiRule, Double)]) extends ComputerPlayer {

  override def takeSquare(implicit board: Board): Board = {
    require(!board.gameOver)

    def humanizedCalcNextMoveOption(implicit board: Board): Option[(Int, Int)] = {
      for (ruleWithOdds <- rulesWithOdds) {
        val rule = ruleWithOdds._1
        val odds = ruleWithOdds._2

        val rnd = random.nextDouble

        if (rnd <= odds) {
          rule.squareToPlay(board) match {
            case None =>
            case Some(square: (Int, Int)) => return Some(square)
          }
        }
      }
      return None
    }

    humanizedCalcNextMoveOption match {
      case None =>
      case Some(square: (Int, Int)) => return board.setCellState(square, icon)
    }
    return board.setCellState(randomEmptySquare, icon)
  }
}