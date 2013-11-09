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
          new ProbableRule(new Priority(icon), level)
    )

    new HumanizedAi(icon, None, rulesWithOdds, Nil)
  }
  
  /*
   * 
   * // TODO:  doesn't know the CornerNearOpponent Rule
   * // TODO:  doesn't know the Priority Rule

      val aiRules = configBuilder.buildAi("is unbeatable");

      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable");

      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random");

      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random, blocks 90% of the time, never misses a win");

      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random, blocks 90% of the time, and never misses a win");
     

    it("supports using whitespace to stack vertically") {
      val aiRules = configBuilder.buildAi("""
          opens with center or corner, 
    		  otherwise is random, 
    		  plays win 90% of the time, 
    		  and never misses a block
          """);

      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable, except misses blocks 10% of the time");
     

      val aiRules = configBuilder.buildAi("is unbeatable, except misses wins 10% of the time");

      val aiRules = configBuilder.buildAi("is unbeatable, misses wins 10%");
 
   */

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