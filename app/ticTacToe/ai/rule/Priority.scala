package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

case class Priority(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    val availableMoves = board.emptySquares
    val priorities = Seq(
      (1, 1),
      (0, 0),
      (0, 2),
      (2, 0),
      (2, 2),
      (1, 0),
      (0, 1),
      (1, 2),
      (2, 1)
    )
    
    priorities.filter(availableMoves.contains(_)).headOption
  }

}
