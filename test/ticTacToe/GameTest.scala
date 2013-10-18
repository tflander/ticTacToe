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
      val winner = reportWinnerFor(board)
      Seq(X, Clear) should contain (winner)
    }
    for (i <- 1 to runsPerTest) play
  }

  it("never loses as O") {
    def play = {
      game = new Game
      val board = game.play(new SmarterAi(X), new SmartestAi(O))

      val winner = reportWinnerFor(board)
      Seq(O, Clear) should contain (winner)
    }
    for (i <- 1 to runsPerTest) play
  }

  it("always ties using the best AI (war games)") {
    def play = {
      game = new Game
      val board = game.play(new SmartestAi(X), new SmartestAi(O))

      val winner = reportWinnerFor(board)
      winner should be(Clear)
    }
    for (i <- 1 to runsPerTest) play
  }

  def reportWinnerFor(board: Board): CellState = {
    val winner = board.winner
    if (winner == Clear) {
      println("\nCat\n")
    } else {
      println("\n" + winner + " wins\n")
    }
    return winner
  }
  
}