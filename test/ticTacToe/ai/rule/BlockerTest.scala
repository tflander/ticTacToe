package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class BlockerTest extends FunSpec with ShouldMatchers {

  describe("when squareToPlay") {
    it("should block a win horizontally") {
      val board = Board()
        .setCellState(1, 0, X)
        .setCellState(2, 0, X)

      val ai = new Blocker(O)
      ai.squareToPlay(board) should be (Some(0, 0))
    }
  }

  describe("when takeSquare") {
    
    it("should block a win horizontally") {
      val board = Board()
        .setCellState(1, 0, X)
        .setCellState(2, 0, X)

      val ai = new Blocker(O)
      val updatedBoard = ai.squareToPlay(board)
      ai.squareToPlay(board) should be (Some(0, 0))
    }
    
    it("should block a win vertically") {
      val board = Board()
        .setCellState(0, 1, X)
        .setCellState(0, 2, X)

      val ai = new Blocker(O)
      val updatedBoard = ai.squareToPlay(board)
      ai.squareToPlay(board) should be (Some(0, 0))
    }
    
    it("should block a win on diagonal one") {
      val board = Board()
        .setCellState(0, 0, X)
        .setCellState(1, 1, X)

      val ai = new Blocker(O)
      val updatedBoard = ai.squareToPlay(board)
      ai.squareToPlay(board) should be (Some(2, 2))
    }

    it("should block a win on diagonal two") {
      val board = Board()
        .setCellState(0, 2, X)
        .setCellState(1, 1, X)

      val ai = new Blocker(O)
      val updatedBoard = ai.squareToPlay(board)
      ai.squareToPlay(board) should be (Some(2, 0))
    }
    
    it("should return None when not threatened with a loss") {
      val board = Board()
        .setCellState(0, 1, X)
        .setCellState(1, 0, X)

      val ai = new Blocker(O)
      val updatedBoard = ai.squareToPlay(board)
      ai.squareToPlay(board) should be (None)      
    }
    
  }

}