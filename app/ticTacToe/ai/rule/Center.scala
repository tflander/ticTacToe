package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

case class Center(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    val availableMoves = board.emptySquares
    if(availableMoves.contains((1, 1))) Some((1, 1)) else None
  }

}
