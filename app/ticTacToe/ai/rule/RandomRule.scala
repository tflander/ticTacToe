package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board
import ticTacToe.ai.ComputerPlayer

case class RandomRule(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    if(board.emptySquares == Nil) return None
    return Some(randomEmptySquare(board))
  }

}
