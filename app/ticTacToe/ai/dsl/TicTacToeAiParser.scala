package ticTacToe.ai.dsl
import ticTacToe.CellState._
import scala.util.parsing.combinator.JavaTokenParsers
import ticTacToe.ai.rule.AiRule
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.RandomRule
import ticTacToe.ai.rule.CenterOrCorner
import ticTacToe.ai.rule.ProbableRule
import ticTacToe.ai.HumanizedAi

class TicTacToeAiParser(icon: CellState) extends JavaTokenParsers {

  def ruleSet: Parser[HumanizedAi] = opt(openingRule <~ ",") ~ primaryRule ~ opt("," ~> exceptionRules) ^^ {
    case openingRule ~ primaryRule ~ exceptionRule => BuildAi(openingRule, primaryRule, exceptionRule)
  }

  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"
  
  def probableException: Parser[ProbableRule] = probableRule ~ probability ^^ { case probableRule ~ probability => buildRule(probableRule, probability) }
  def probableRule: Parser[String] = "misses wins" | "misses blocks" | "wins" | "blocks" | "plays win"
  
  def removeFromPrimaryRulesDecoratorBefore: Parser[String] = "misses the" | "except misses the"
  def removeFromPrimaryRulesDecoratorAfter: Parser[String] = "rule"
  def ruleToRemove = "corner near opponent" | "priority"
  def removeFromPrimaryRule: Parser[ProbableRule] = removeFromPrimaryRulesDecoratorBefore ~> ruleToRemove <~ removeFromPrimaryRulesDecoratorAfter  ^^ (buildRuleToRemove(_))   
    
  def simpleException: Parser[AiRule] = simpleExceptionRule ^^ (buildRule(_))
  def simpleExceptionRule: Parser[String] = "never misses a win" | "never misses a block"

  def exception: Parser[AiRule] = simpleException | probableException | removeFromPrimaryRule
  def exceptionRule: Parser[AiRule] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[AiRule]] = repsep(exceptionRule, ",")

  def primaryRuleName: Parser[String] = "random" | "unbeatable"
  def primaryRule: Parser[Seq[AiRule]] = primaryRuleDecorator ~> primaryRuleName ^^ (buildPrimaryRule(_))
  def primaryRuleDecorator: Parser[String] = "is" | "otherwise is" | ""

  def openingRule: Parser[AiRule] = openingDecorator ~> openingRuleName ^^ (buildOpeningRule(_))
  def openingRuleName: Parser[String] = "randomly" | "with center or corner"
  def openingDecorator: Parser[String] = "opens" | ""
  
  def BuildAi(openingRule: Option[AiRule], primaryRule: Seq[AiRule], exceptionRule: Option[List[AiRule]]) = {
    val exceptions = exceptionRule match {
      case Some(rules: List[AiRule]) => rules
      case None => Nil
    }
    new HumanizedAi(icon, openingRule, primaryRule, exceptions)
  }
  
  def buildRuleToRemove(rule: String) = {
    rule match {
      case "corner near opponent" => new ProbableRule(new CornerNearOpponent(icon), 0.0) 
      case "priority" => new ProbableRule(new Priority(icon), 0.0) 
    }
  }

  def buildOpeningRule(rule: String) = {
    rule match {
      case "randomly" => new RandomRule(icon)
      case "with center or corner" => new CenterOrCorner(icon)
    }
  }

  def buildRule(probableRule: String, probability: Double) = {
    probableRule match {
      case "misses wins" => new ProbableRule(new Winner(icon), 1 - probability)
      case "misses blocks" => new ProbableRule(new Blocker(icon), 1 - probability)
      case "wins" => new ProbableRule(new Winner(icon), probability)
      case "plays win" => new ProbableRule(new Winner(icon), probability)
      case "blocks" => new ProbableRule(new Blocker(icon), probability)
      // case "misses the corner near opponent rule" => new ProbableRule(new CornerNearOpponent(icon), 0.0)
    }
  }

  def buildPrimaryRule(rule: String) = {
    rule match {
      case "unbeatable" => {
        Seq(
          new Opener(icon),
          new Winner(icon),
          new Blocker(icon),
          new CornerNearOpponent(icon),
          new Priority(icon))
      }
      case "random" => {
        Seq(new RandomRule(icon))
      }
    }
  }

  def buildRule(rule: String): AiRule = {
    rule match {
      case "never misses a win" => new Winner(icon)
      case "never misses a block" => new Blocker(icon)
    }
  }
}

