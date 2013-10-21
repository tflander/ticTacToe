package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

trait LineAi {

  def winningPosition(cells: Seq[CellState]): Int = {
    for (i <- 0 to cells.size) {
      if (cells(i) == Clear) return i
    }
    throw new IllegalArgumentException("no clear cell available")
  }

  def rowRule(line: Seq[CellState], i: Int) = (winningPosition(line), i)
  def columnRule(line: Seq[CellState], i: Int) = (i, winningPosition(line))
  def diag1Rule(line: Seq[CellState], i: Int) = (winningPosition(line), winningPosition(line))
  def diag2Rule(line: Seq[CellState], i: Int) = (winningPosition(line), (line.size - 1) - winningPosition(line))

  def buildRulesForBoard(rule: Seq[CellState] => Boolean, board: Board): Seq[Rule] = {
    Seq(
      new Rule(rule, board.rows, rowRule),
      new Rule(rule, board.columns, columnRule),
      new Rule(rule, Seq(board.diagonalOne), diag1Rule),
      new Rule(rule, Seq(board.diagonalTwo), diag2Rule))
  }

  class Rule(
    val shouldPlayLine: Seq[CellState] => Boolean,
    val linesToCheck: Seq[Seq[CellState]],
    val moveIfShouldPlay: (Seq[CellState], Int) => (Int, Int))

  def applyRule(rule: Rule): Option[(Int, Int)] = {

    for (i <- 0 to rule.linesToCheck.size - 1) {
      val line = rule.linesToCheck(i)
      if (rule.shouldPlayLine(line)) {
        return Some(rule.moveIfShouldPlay(line, i))
      }
    }
    return None
  }

  def move(rules: Seq[Rule]): Option[(Int, Int)] = {
    for (rule <- rules) {
      val winningPositionOrNot = applyRule(rule)
      if (winningPositionOrNot != None) {
        return winningPositionOrNot
      }
    }
    return None
  }
}