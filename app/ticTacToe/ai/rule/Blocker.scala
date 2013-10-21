package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Blocker(icon: CellState) extends AiRule with LineAi {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    /*
     * ideal DSL:
     * 
     *  return move to block 
     * 
     */
    
    def mustIBlockThisTurn = canWinThisTurn(board, opponent)(_)
    return move(buildRulesForBoard(mustIBlockThisTurn, board));

  }

}