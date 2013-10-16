package ticTacToe.ai

import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.CornerNearOpponent

class SmartestAi(icon: CellState) extends ComputerPlayer {

  val rules = Seq(
    new Opener(icon),
    new Winner(icon),
    new Blocker(icon),
    new CornerNearOpponent(icon),
    new Priority(icon))

  override def takeSquare(implicit board: Board): Board = {
    require(!board.gameOver)

    return board.setCellState(calcNextMove(rules), icon)
  }
}