package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class OpenerTest extends FunSpec with ShouldMatchers {

  describe("First move of the game") {
    it("never picks an edge.  It always picks center or a corner") {
      for (i <- 1 to 10) {
        val board = Board()
        val validOpeners = Seq(
          Some(0, 0),
          Some(2, 0),
          Some(0, 2),
          Some(2, 2),
          Some(1, 1))

        val ai = new Opener(X)
        validOpeners should contain(ai.squareToPlay(board))
      }
    }
  }

  describe("Moves second") {
    it("takes the center if opponent takes a corner") {
      val board = Board().setCellState(0, 0, X)
      val ai = new Opener(O)
      ai.squareToPlay(board) should be(Some(1, 1))
    }

    it("takes the center if opponent takes an edge") {
      val board = Board().setCellState(1, 0, X)
      val ai = new Opener(O)
      ai.squareToPlay(board) should be(Some(1, 1))
    }

    it("takes a corner if the opponent takes the center") {
      val corners = Seq(
        Some(0, 0),
        Some(2, 0),
        Some(0, 2),
        Some(2, 2))
      val board = Board().setCellState(1, 1, X)
      val ai = new Opener(O)
      corners should contain(ai.squareToPlay(board))
    }
  }

  describe("Moves third") {
    it("takes opposite corner if have center and opponent took corner") {
      val board = Board()
        .setCellState(1, 1, X)
        .setCellState(0, 0, O)
      val ai = new Opener(X)
      ai.squareToPlay(board) should be(Some(2, 2))
    }

    it("takes adjacent corner if have corner and opponent took center") {
      val board = Board()
        .setCellState(0, 0, X)
        .setCellState(1, 1, O)
      val ai = new Opener(X)
      ai.squareToPlay(board) should be(Some(0, 2))
    }

    it("takes center if opponent took an edge") {
      val board = Board()
        .setCellState(0, 0, X)
        .setCellState(0, 1, O)
      val ai = new Opener(X)
      ai.squareToPlay(board) should be(Some(1, 1))
    }
  }

  describe("moves 4th") {
    it("takes edge if have center and opponent has opposite corners") {

      val board = Board(
        (Clear, Clear, X),
        (Clear, O, Clear),
        (X, Clear, Clear))

      val ai = new Opener(O)
      ai.squareToPlay(board) should be(Some(0, 1)) // any edge is fine
    }
  }
}