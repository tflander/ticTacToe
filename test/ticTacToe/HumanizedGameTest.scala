package ticTacToe

import org.scalatest._
import CellState._
import ticTacToe.ai.SmartestAi
import ticTacToe.ai.SmarterAi
import ticTacToe.ai.HumanizedAi
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.Priority

class HumanizedGameTest extends FunSpec with ShouldMatchers {

  var game: Game = _
  val runsPerTest = 10000

  def humanizedPlayer(icon: CellState, level: Double) = {
    val rulesWithOdds = Seq(
          (new Opener(icon), level), 
          (new Winner(icon), level), 
          (new Blocker(icon), level), 
          (new CornerNearOpponent(icon), level),
          (new Priority(icon), level)
    )

    new HumanizedAi(icon, rulesWithOdds)
  }

  it("shows humanized stats") {
    def play: CellState = {
      game = new Game
      val board = game.play(humanizedPlayer(X, 0.1), humanizedPlayer(O, 0.3))
      return board.winner
    }
    val stats = for (i <- 1 to runsPerTest) yield play
    val wins = stats.count(_ == X)
    val losses = stats.count(_ == O)
    val ties = stats.count(_ == Clear)

    println("wins = " + wins + " " + wins * 100 / stats.size + "%")
    println("losses = " + losses + " " + losses * 100 / stats.size + "%")
    println("ties = " + ties + " " + ties * 100 / stats.size + "%")
  }

}