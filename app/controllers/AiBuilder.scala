package controllers

import play.api._
import play.api.mvc._
import ticTacToe.ai.dsl.AiBuilder._

object AiBuilder extends Controller {

  def index = Action {
    Ok(views.html.aiBuilder(usage, "", ""))
  }
  
  def execRule(rule: String) = Action {
    Ok(views.html.aiBuilder(usage, rule, "todo:  results"))
  }
}