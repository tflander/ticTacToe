package controllers

import play.api._
import play.api.mvc._
import ticTacToe.CellState._
import ticTacToe.ai.dsl.AiBuilder._
import controllers.support.AiBuilderViewParams
import ticTacToe.AiVersesAiMatchRunner._

object AiBuilder extends Controller {

  val defaultResult = (true, "")

  val sampleRules = Seq(
    "is unbeatable, except misses wins 10% of the time",
    "is unbeatable, except misses the priority rule",
    "is unbeatable, except misses the cornerNearOpponent rule",
    "is unbeatable",
    "opens randomly, otherwise is unbeatable",
    "opens with centerOrCorner, otherwise is random",
    "opens with centerOrCorner, otherwise is random, blocks 90% of the time, and never misses a win",
    "opens randomly, otherwise is unbeatable, except misses blocks 10% of the time",
    "random")

  def index = Action {

    val params = AiBuilderViewParams(
      sampleRules = sampleRules,
      usage = usage)

    Ok(views.html.aiBuilder(params))
  }

  def execRule(rule: String) = Action {

    val params = AiBuilderViewParams(
      sampleRules = sampleRules,
      usage = usage,
      xRule = rule,
      xResult = ruleResult(X, rule))

    Ok(views.html.aiBuilder(params))
  }

  def execRules(xRule: String, oRule: String) = Action {

    val xResult = ruleResult(X, xRule)
    val oResult = ruleResult(O, oRule)
    
    def run(xRule: String, oRule: String): String = {
    	val x = buildAi(X, xRule)
    	val o = buildAi(O, oRule)
    	
    	def roundsPerMatch = 100
    	runMatch(roundsPerMatch, (xRule, oRule))
    		.map(score => score.player + " w/l/t " + score.wins + "/" + score.losses + "/" + score.ties + " " + score.points + " points")
    		.mkString("<br />")
    }
    
    val matchResults = if(xResult._1 && oResult._1) run(xRule, oRule) else ""

    val params = AiBuilderViewParams(
      sampleRules = sampleRules,
      usage = usage,
      xRule = xRule,
      oRule = oRule,
      xResult = xResult,
      oResult = oResult,
      matchResults = matchResults)

    Ok(views.html.aiBuilder(params))
  }

  def ruleResult(icon: CellState, rule: String) = {
    try {
      (true, buildAi(icon, rule).toString)
    } catch {
      case e: IllegalArgumentException => (false, e.getMessage())
    }
  }

}