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

    it("should allow you to copy a board with a cell change via two Ints") {
      val clearBoard = Board()
      val updatedBoard = clearBoard.setCellState(1, 2, X)
      updatedBoard.cellState(1, 2) should be(X)
    }

    it("should allow you to copy a board with a cell change via a tuple of Ints") {
      val clearBoard = Board()
      val updatedBoard = clearBoard.setCellState((1, 2), X)
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
    
    it("should construct a populated board") {
      val board = Board(
        (Clear, Clear, Clear),
        (X, X, X),
        (O, O, O))

      board.cellState(0, 0) should be(Clear)
      board.cellState(1, 0) should be(Clear)
      board.cellState(2, 0) should be(Clear)

      board.cellState(0, 1) should be(X)
      board.cellState(1, 1) should be(X)
      board.cellState(2, 1) should be(X)

      board.cellState(0, 2) should be(O)
      board.cellState(1, 2) should be(O)
      board.cellState(2, 2) should be(O)
    }

  }

  describe("line checks") {
    
    it("should give you rows") {
      val board = Board(
        (Clear, Clear, Clear),
        (X, X, X),
        (O, O, O))

      board.rows should be (Seq(
       Seq(Clear, Clear, Clear),
       Seq(X, X, X),
       Seq(O, O, O)
      ))
    }

    it("should give you columns") {
      val board = Board(
        (Clear, Clear, Clear),
        (X, X, X),
        (O, O, O))

      board.columns should be (Seq(
       Seq(Clear, X, O),
       Seq(Clear, X, O),
       Seq(Clear, X, O)
      ))
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
      val updatedBoard = Board(
        (X, Clear, Clear),
        (Clear, X, Clear),
        (Clear, Clear, X))

      updatedBoard.winner should be(X)
    }

    it("know when X wins on diagonal two") {
      val board = Board()

      val updatedBoard = Board(
        (Clear, Clear, X),
        (Clear, X, Clear),
        (X, Clear, Clear))

      updatedBoard.winner should be(X)
    }
  }

  describe("turn management") {
    it("counts the number of turns played") {

      val board = Board()

      val updatedBoard = Board(
        (O, Clear, Clear),
        (Clear, X, Clear),
        (Clear, Clear, X))

      board.turnsPlayed should be(0)
      updatedBoard.turnsPlayed should be(3)
    }
  }

  it("knows if the game is over") {
    val emptyboard = Board()

    val boardForGameInProgress = Board(
      (X, O, X),
      (O, X, O),
      (O, Clear, Clear))

    val boardWinnerIsX = Board(
      (X, O, X),
      (O, X, O),
      (X, Clear, Clear))

    val completedBoardToTie = Board(
      (X, O, X),
      (O, X, O),
      (O, X, O))

    emptyboard.gameOver should be(false)
    boardForGameInProgress.gameOver should be(false)
    completedBoardToTie.gameOver should be(true)
    boardWinnerIsX.gameOver should be(true)
  }

  describe("occupied squares") {
    it("should find them") {

      val board = Board(
        (O, Clear, Clear),
        (Clear, X, Clear),
        (Clear, Clear, X))

      board.occupiedSquares(X) should be(Seq((1, 1), (2, 2)))
    }
  }

  describe("empty squares") {
    it("should find them") {
      val board = Board()
        .setCellState(1, 1, X)
        .setCellState(2, 2, X)

      board.emptySquares should be(Seq(
        (0, 0),
        (0, 1),
        (0, 2),
        (1, 0),
        (1, 2),
        (2, 0),
        (2, 1)))
    }
  }

  describe("when nextPlayer") {
    it("knows X goes first") {
      val board = Board()
      board.nextPlayer should be(X)
    }

    it("knows O goes second") {
      val board = Board().setCellState((1, 1), X)
      board.nextPlayer should be(O)
    }

  }

}