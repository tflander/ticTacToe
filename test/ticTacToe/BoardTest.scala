package ticTacToe
import org.scalatest._
import CellState._

class BoardTest extends FunSpec with ShouldMatchers {

  describe("board manipulation") {
    it("should create an empty board") {
      val board = Board()

      for (row <- 0 to 2)
        for (col <- 0 to 2)
          board.cellState(col, row) should be(Clear)
    }

    it("should allow you to copy a board with a cell change") {
      val clearBoard = Board()
      val updatedBoard = clearBoard.setCellState(1, 2, X)
      updatedBoard.cellState(1, 2) should be(X)
    }

    it("has no winner for a new board") {
      val board = Board()
      board.winner should be(Clear)
    }
    
    it("fails when getting a cell out of bounds") {
      val board = Board()
      intercept[ArrayIndexOutOfBoundsException] {
    	  board.cellState(3, 3)
      }
    }
    
    it("fails when setting a cell out of bounds") {
      val board = Board()
      intercept[IllegalArgumentException] {
        board.setCellState(3, 3, X)
      }
    }
  }

  describe("3 in a row horizontally") {

    it("knows X wins when three X's in any row") {
      for (row <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, row, X)
          .setCellState(1, row, X)
          .setCellState(2, row, X)
        updatedBoard.winner should be(X)
      }
    }

    it("knows O wins when three O's in any row") {
      for (row <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, row, O)
          .setCellState(1, row, O)
          .setCellState(2, row, O)
        updatedBoard.winner should be(O)
      }
    }

    it("knows no winner when only 2 X's in any row") {
      for (row <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, row, X)
          .setCellState(1, row, X)
        updatedBoard.winner should be(Clear)
      }
    }

    it("knows no winner when O blocks X in any row") {
      for (row <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, row, X)
          .setCellState(1, row, X)
          .setCellState(2, row, O)
        updatedBoard.winner should be(Clear)
      }
    }
  }
  
  describe("3 in a row vertically") {

    it("knows X wins when three X's in any column") {
      for (col <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(col, 0, X)
          .setCellState(col, 1, X)
          .setCellState(col, 2, X)
        updatedBoard.winner should be(X)
      }
    }

    it("knows O wins when three O's in any column") {
      for (col <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(col, 0, O)
          .setCellState(col, 1, O)
          .setCellState(col, 2, O)
        updatedBoard.winner should be(O)
      }
    }

    it("knows no winner when only 2 X's in any column") {
      for (col <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(col, 0, X)
          .setCellState(col, 1, X)
        updatedBoard.winner should be(Clear)
      }
    }

    it("knows no winner when O blocks X in any column") {
      for (col <- 0 to 2) {
        val board = Board()
        val updatedBoard = board
          .setCellState(col, 0, X)
          .setCellState(col, 1, X)
          .setCellState(col, 2, O)
        updatedBoard.winner should be(Clear)
      }
    }
  }
  
  describe("diagionals tests") {

    it("know when X wins on diagonal one") {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, 0, X)
          .setCellState(1, 1, X)
          .setCellState(2, 2, X)
        updatedBoard.winner should be(X)
    }
    
    it("know when X wins on diagonal two") {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, 2, X)
          .setCellState(1, 1, X)
          .setCellState(2, 0, X)
        updatedBoard.winner should be(X)
    }
  }
  
  describe("turn management") {
    it("counts the number of turns played") {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, 2, X)
          .setCellState(1, 1, O)
          .setCellState(2, 0, X)
          
        board.turnsPlayed should be(0)
        updatedBoard.turnsPlayed should be(3)
  	}
  }
  
  it("knows if the game is over") {
        val board = Board()
        val updatedBoard = board
          .setCellState(0, 0, X)
          .setCellState(0, 1, O)
          .setCellState(0, 2, X)
        val completedBoard = updatedBoard
          .setCellState(1, 0, X)
          .setCellState(1, 1, O)
          .setCellState(1, 2, X)
          .setCellState(2, 0, X)
          .setCellState(2, 1, O)
          .setCellState(2, 2, X)
          
        board.gameOver should be(false)
        updatedBoard.gameOver should be(false)
        completedBoard.gameOver should be(true)
  }
  
  describe("occupied squares") {
    it("should find them") {
      val board = Board()
      .setCellState(1, 1, X)
      .setCellState(2, 2, X)
      
      board.occupiedSquares(X) should be (Seq((1, 1), (2, 2)))
    }
  }
}