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
import ticTacToe.ai.dsl.TicTacToeAiParser
import AiVersesAiMatchRunner._

class HumanizedGameTest extends FunSpec with ShouldMatchers {

  var game: Game = _
  val roundsPerMatch = 100

  val computerPlayerRules = Seq(
    "is unbeatable, except misses the priority rule",
    "is unbeatable, except misses the cornerNearOpponent rule",
    "is unbeatable",
    "opens randomly, otherwise is unbeatable",
    "opens with centerOrCorner, otherwise is random",
    "opens with centerOrCorner, otherwise is random, blocks 90% of the time, and never misses a win",
    "opens randomly, otherwise is unbeatable, except misses blocks 10% of the time",
    "is unbeatable, except misses wins 10% of the time")

  ignore("should round robin players") {
    import RoundRobin._
	val tournament = buildSchedule(computerPlayerRules)
    	
	val rawResults = for (matchUp <- tournament) yield runMatch(roundsPerMatch, matchUp)
    println(rawResults)
    println("====")
    val results = rawResults.flatten.groupBy(_.player)
    	.mapValues(tupleList => tupleList.map(_.points))
    	.mapValues(scoreList => scoreList.foldLeft(0)(_ + _))
    println(results)
  }
  
  ignore("should run a single match") {
    import RoundRobin._
    val results = runMatch(roundsPerMatch, ("is random, never misses a block, never misses a win", "opens randomly, otherwise is unbeatable"))
    println(results)
  }

  
}