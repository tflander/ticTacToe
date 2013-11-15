package controllers

import play.api._
import play.api.mvc._
import ticTacToe.CellState._
import ticTacToe.ai.dsl.AiBuilder._
import controllers.support.AiBuilderViewParams

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
    
    val params = AiBuilderViewParams (
		sampleRules = sampleRules, 
		usage = usage
    )
    
    Ok(views.html.aiBuilder(params))
  }

  def execRule(rule: String) = Action {
    
    val params = AiBuilderViewParams (
		sampleRules = sampleRules, 
		usage = usage,
		xRule = rule, 
		xResult = ruleResult(rule)
    )
    
    Ok(views.html.aiBuilder(params))
  }

  def execRules(xRule: String, oRule: String) = Action {

    val params = AiBuilderViewParams (
		sampleRules = sampleRules, 
		usage = usage,
		xRule = xRule, 
		oRule = oRule, 
		xResult = ruleResult(xRule),
		oResult = ruleResult(oRule),
		matchResults = "todo: build match results"
    )

    Ok(views.html.aiBuilder(params))
  }

  def ruleResult(rule: String) = {
    try {
      (true, buildAi(X, rule).toString)
    } catch {
      case e: IllegalArgumentException => (false, e.getMessage())
    }
  }

}