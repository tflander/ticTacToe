package ticTacToe.ai

import ticTacToe.Board
import ticTacToe.ai.rule.AiRule
import scala.util.Random
import java.util.Date
import ticTacToe.CellState._

trait ComputerPlayer {
  
  val random = new Random(new Date().getTime)

  def takeSquare(implicit board: Board): Board

  def calcNextMove(rules: Seq[AiRule])(implicit board: Board): (Int, Int) = {
    calcNextMoveOption(rules).get
  }

  def calcNextMoveOption(rules: Seq[AiRule])(implicit board: Board): Option[(Int, Int)] = {
    for (rule <- rules) {
      rule.squareToPlay(board) match {
        case None =>
        case Some(square: (Int, Int)) => return Some(square)
      }
    }
    return None
  }

  def randomEmptySquare(implicit board: Board): (Int, Int) = {

    while (true) {
      val col = random.nextInt(board.boardSize)
      val row = random.nextInt(board.boardSize)
      if (board.cellState(col, row) == Clear) {
        return (col, row)
      }
    }

    (0, 0)
  }

}