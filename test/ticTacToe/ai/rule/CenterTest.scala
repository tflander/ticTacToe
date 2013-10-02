package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class CenterTest extends FunSpec with ShouldMatchers {

  it("should take center if available") {
    val board = Board()

    val ai = new Center(X)
    ai.squareToPlay(board) should be(Some(1, 1))
  }
  
  it("returns None if center occupied") {
    val board = Board().setCellState((1, 1), X)

    val ai = new Center(O)
    ai.squareToPlay(board) should be(None)
  }  

}