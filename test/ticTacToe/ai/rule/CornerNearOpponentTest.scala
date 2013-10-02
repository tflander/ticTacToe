package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class CornerNearOpponentTest extends FunSpec with ShouldMatchers {

  it("should take corner near opponent if available") {
    val board = Board()
      .setCellState((2, 1), X)
      .setCellState((1, 1), O)
      .setCellState((1, 2), X)

    val ai = new CornerNearOpponent(O)
    ai.squareToPlay(board) should be(Some(0, 2))
  }

}