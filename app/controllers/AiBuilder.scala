package controllers

import play.api._
import play.api.mvc._
import ticTacToe.CellState._
import ticTacToe.ai.dsl.AiBuilder._

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
	   		"opens randomly, otherwise is unbeatable, except misses blocks 10% of the time"
	   	)

  def index = Action {
    Ok(views.html.aiBuilder(sampleRules, usage, "", "", defaultResult, defaultResult))
  }

  def execRule(rule: String) = Action {
    Ok(views.html.aiBuilder(sampleRules, usage, rule, "", ruleResult(rule), defaultResult))
  }

  def execRules(xRule: String, oRule: String) = Action {
    Ok(views.html.aiBuilder(sampleRules, usage, xRule, oRule, ruleResult(xRule), ruleResult(oRule)))
  }

  def ruleResult(rule: String) = {
    try {
      (true, buildAi(X, rule).toString)
    } catch {
      case e: IllegalArgumentException => (false, e.getMessage())
    }
  }

}