package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class PriorityTest extends FunSpec with ShouldMatchers {

  describe("when squareToPlay") {

    it("should take center if available") {
      val board = Board()

      val ai = new Priority(X)
      ai.squareToPlay(board) should be(Some(1, 1))
    }

    it("should take a corner if the center is occupied") {
      val board = Board().setCellState((1, 1), X)

      val ai = new Priority(O)
      ai.squareToPlay(board) should be(Some(0, 0))
    }

    it("should take an edge if the center and corners are occupied") {

      val board = Board(
        (X, Clear, X),
        (Clear, X, Clear),
        (X, Clear, X))

      val ai = new Priority(O)
      ai.squareToPlay(board) should be(Some(1, 0))
    }
  }

}