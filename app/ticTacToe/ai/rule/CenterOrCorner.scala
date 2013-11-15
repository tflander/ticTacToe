package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

case class CenterOrCorner(icon: CellState) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    val availableMoves = board.emptySquares
    if(availableMoves.isEmpty) return None

    while (true) {
      val candidate = random.nextInt(5) match {
        case 0 => takeCenter()
        case _ => takeCorner()
      }
      
      if(availableMoves.contains(candidate.get))
        return candidate
    }
    None
  }
}
