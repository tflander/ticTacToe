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
import ticTacToe.ai.rule.ProbableRule

class HumanizedGameTest extends FunSpec with ShouldMatchers {

  var game: Game = _
  val runsPerTest = 10000

  def humanizedPlayer(icon: CellState, level: Double) = {
    val rulesWithOdds = Seq(
      new ProbableRule(new Opener(icon), level),
      new ProbableRule(new Winner(icon), level),
      new ProbableRule(new Blocker(icon), level),
      new ProbableRule(new CornerNearOpponent(icon), level),
      new ProbableRule(new Priority(icon), level))

    new HumanizedAi(icon, None, rulesWithOdds, Nil)
  }

  val computerPlayerRules = Seq(
    "is unbeatable, except misses the priority rule",
    "is unbeatable, except misses the corner near opponent rule",
    "is unbeatable",
    "opens randomly, otherwise is unbeatable",
    "opens with center or corner, otherwise is random",
    "opens with center or corner, otherwise is random, blocks 90% of the time, and never misses a win",
    "opens randomly, otherwise is unbeatable, except misses blocks 10% of the time",
    "is unbeatable, except misses wins 10% of the time")

  it("shows humanized stats") {
    def play: CellState = {
      game = new Game
      val board = game.play(humanizedPlayer(X, 0.1), humanizedPlayer(O, 1.0))
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