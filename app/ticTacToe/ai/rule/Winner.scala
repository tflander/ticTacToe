package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

case class Winner(icon: CellState) extends AiRule with LineAi {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    /*
     * ideal DSL:
     * 
     *  return move to win 
     * 
     */
    def canIWinThisTurn = canWinThisTurn(board, icon)(_)
    return move(buildRulesForBoard(canIWinThisTurn, board));
  }
  
}
