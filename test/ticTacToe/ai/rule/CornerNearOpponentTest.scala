package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class CornerNearOpponentTest extends FunSpec with ShouldMatchers {

  it("should take corner near opponent if available") {

    val board = Board(
      (Clear, X,     Clear),
      (Clear, O,     X    ),
      (Clear, Clear, Clear))

    val ai = new CornerNearOpponent(O)
    ai.squareToPlay(board) should be(Some(0, 0))
  }

}