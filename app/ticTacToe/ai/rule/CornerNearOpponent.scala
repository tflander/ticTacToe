package ticTacToe.ai.rule

import ticTacToe.CellState._
import ticTacToe.Board

class CornerNearOpponent(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X
  val corners = Seq(
    (0, 0),
    (0, 2),
    (2, 0),
    (2, 2))

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    def isEdge()(square: (Int, Int)): Boolean = {
      return (square._1 == 1 || square._2 == 1) && square._1 != square._2
    }

    def isCorner()(square: (Int, Int)): Boolean = {
      return corners.contains(square)
    }

    println(board.occupiedSquares(opponent))
    val opponentsEdgeMoves = board.occupiedSquares(opponent).filter(isEdge())
    val availableCorners = board.emptySquares.filter(isCorner())
    for (corner <- availableCorners) {
      for(edge <- opponentsEdgeMoves) {
        if(corner._1 == edge._1 || corner._2 == edge._2) {
          return Some(corner)
        }
      }
    }
    return None
  }
}
