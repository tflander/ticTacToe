package ticTacToe.ai
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class SmarterAiTest extends FunSpec with ShouldMatchers {

  describe("when takeSquare") {
    
    it("should win if possible") {
      val board = Board()
        .setCellState(1, 0, X)
        .setCellState(2, 0, X)

      val ai = new SmarterAi(X)
      val updatedBoard = ai.takeSquare(board)
      updatedBoard.cellState(0, 0) should be(X)
      updatedBoard.winner should be(X)
    }
    
    it("should block a win vertically") {
      val board = Board()
        .setCellState(0, 1, X)
        .setCellState(0, 2, X)

      val ai = new SmarterAi(O)
      val updatedBoard = ai.takeSquare(board)
      updatedBoard.cellState(0, 0) should be(O)
    }
        
  }

}