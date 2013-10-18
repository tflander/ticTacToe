package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Winner(icon: CellState) extends AiRule with LineAi {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    def canIWinThisTurn = canWinThisTurn(board, icon)(_)
    return moveFromRulesOrNone(buildRulesForBoard(canIWinThisTurn, board));
  }
  
}
