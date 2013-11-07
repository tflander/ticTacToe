package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class CenterOrCornerTest extends FunSpec with ShouldMatchers {
  
  val ai = new CenterOrCorner(X)
  
  it("picks a random corner or center on an empty board") {
    val board = Board()
    ai.squareToPlay(board) should not be (None)
  }

  it("returns none on a full board") {
      val board = Board(
        (O, X, X),
        (O, X, O),
        (O, O, X))
    ai.squareToPlay(board) should be (None)
  }

  it("finds the last empty corner") {
      val board = Board(
        (O, X, Clear),
        (O, X, O),
        (O, O, X))
    ai.squareToPlay(board) should be (Some(2,0))
  }
}