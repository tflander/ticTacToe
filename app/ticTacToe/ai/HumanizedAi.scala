package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.AiRule
import ticTacToe.ai.dsl.TicTacToeAiParser
import ticTacToe.ai.rule.ProbableRule

class HumanizedAi(val icon: CellState, val openingRule: Option[AiRule], val primaryRules: Seq[AiRule], val exceptionRules: Seq[AiRule]) extends ComputerPlayer {
  require(!primaryRules.isEmpty, "Primary Ai Rules are required.  Found an empty list")
  
  def isProbableRule()(rule: AiRule) = rule.isInstanceOf[ProbableRule]
  
  val exceptionsThatCanOverridePrimaryRule = exceptionRules.filter(isProbableRule()).map { rule =>
    val probableRule = rule.asInstanceOf[ProbableRule]
    probableRule.baseRule.getClass.getSimpleName
  }
  
  def removeRuleIfHasException()(rule: AiRule): Boolean = {
    return !exceptionsThatCanOverridePrimaryRule.contains(rule.getClass.getSimpleName)
  }
  
  val primaryRulesExceptionsRemoved = primaryRules.filter(removeRuleIfHasException())

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
      for (rule <- primaryRulesExceptionsRemoved) {
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
  
  override def toString() = {
    "icon=" + icon + "\n" +
    "opening=" + openingRule + "\n" +
    "exceptionRules=" + exceptionRules + "\n" +
    "primaryRules=" + primaryRulesExceptionsRemoved
  }
}