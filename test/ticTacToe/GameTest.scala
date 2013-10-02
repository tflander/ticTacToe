package ticTacToe

import org.scalatest._
import CellState._
import ticTacToe.ai.SmartestAi
import ticTacToe.ai.SmarterAi

class GameTest extends FunSpec with ShouldMatchers {

  var game: Game = _
  val runsPerTest = 10000

  it("never loses as X") {
    def play = {
      game = new Game
      val board = game.play(new SmartestAi(X), new SmarterAi(O))

      val winner = board.winner
      if (winner == Clear) {
        println("\nCat\n")
      } else {
        println("\n" + winner + " wins\n")
        winner should be(X)
      }
    }
    for (i <- 1 to runsPerTest) play
  }

  it("never loses as O") {
    def play = {
      game = new Game
      val board = game.play(new SmarterAi(X), new SmartestAi(O))

      val winner = board.winner
      if (winner == Clear) {
        println("\nCat\n")
      } else {
        println("\n" + winner + " wins\n")
        winner should be(O)
      }
    }
    for (i <- 1 to runsPerTest) play
  }

  it("always ties using the best AI (war games)") {
    def play = {
      game = new Game
      val board = game.play(new SmartestAi(X), new SmartestAi(O))

      val winner = board.winner
      winner should be(Clear)
    }
    for (i <- 1 to runsPerTest) play
  }
  
}