package ticTacToe

import org.scalatest._
import CellState._

class GameTest extends FunSpec with ShouldMatchers {

  var game: Game = _

  ignore("never loses as X") {
    def play = {
      game = new Game
      val board = game.play()

      for (row <- 0 to 2) {
        for (col <- 0 to 2) {
          val mark = board.cellState(col, row)
          val icon = if (mark == Clear) "." else mark.toString
          print(icon)
        }
        println("")
      }
      val winner = board.winner
      if (winner == Clear) {
        println("\nCat\n")
      } else {
        println("\n" + winner + " wins\n")
        winner should be(X)
      }
    }
    for (i <- 1 to 100000) play
  }

  it("never loses as O") {
    def play = {
      game = new Game
      val board = game.play()

      val winner = board.winner
      if (winner == Clear) {
        println("\nCat\n")
      } else {
        println("\n" + winner + " wins\n")
        winner should be(O)
      }
    }
    for (i <- 1 to 100000) play
  }
  
}