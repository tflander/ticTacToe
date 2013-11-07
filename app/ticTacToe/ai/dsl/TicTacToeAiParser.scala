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

class TicTacToeAiParser(icon: CellState) extends JavaTokenParsers {
/*
 ai ::= ruleSet [, ruleSet...]
 ruleSet = [openingRule, ] primaryRule [, exceptionRules]
 x openingRule ::= "opens" ("with center or corner" | "randomly")
 x primaryRule ::= "random" | "unbeatable"
 x exceptionRules ::= exceptionRule [, exceptionRule...]
 x exceptionRule ::= ("except" | "" | "and") exception
 x exception ::= simpleException | probableException
 x probableException ::= ("misses wins" | "misses blocks") probability
 x probability ::= floatingPointNunber"%[ of the time]"
 x simpleException ::= "never misses a win" | "never misses a block"
 */
  def ai: Parser[Any] = "TBD"
  def ruleSet: Parser[Any] = opt(openingRule <~ "," ~ opt("otherwise")) ~ primaryRule ~ opt("," ~> repsep(exception, "," ~ opt("and")))

  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"
  def probableException: Parser[(AiRule, Double)] = probableRule ~ probability ^^ { case probableRule ~ probability => buildRule(probableRule, probability) }
  def probableRule: Parser[String] = "misses wins" | "misses blocks" | "wins" | "blocks"

  def simpleException: Parser[AiRule] = simpleExceptionRule ^^ (buildRule(_))
  def simpleExceptionRule: Parser[String] = "never misses a win" | "never misses a block"

  def exception: Parser[Any] = simpleException | probableException
  def exceptionRule: Parser[Any] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[Any]] = repsep(exceptionRule, ",")

  def primaryRuleName: Parser[String] = "random" | "unbeatable"
  def primaryRule: Parser[Seq[AiRule]] = primaryRuleDecorator ~> primaryRuleName ^^ (buildPrimaryRule(_))
  def primaryRuleDecorator: Parser[String] = "is" | ""

  def openingRule: Parser[AiRule] = openingDecorator ~> openingRuleName ^^ (buildOpeningRule(_))
  def openingRuleName: Parser[String] = "randomly" | "with center or corner"
  def openingDecorator: Parser[String] = "opens" | ""

  def buildOpeningRule(rule: String) = {
    rule match {
      case "randomly" => new RandomRule(icon)
      case "with center or corner" => new CenterOrCorner(icon)
    }
  }

  def buildRule(probableRule: String, probability: Double) = {
    probableRule match {
      case "misses wins" => (new Winner(icon), 1 - probability)
      case "misses blocks" => (new Blocker(icon), 1 - probability)
      case "wins" => (new Winner(icon), probability)
      case "blocks" => (new Blocker(icon), probability)
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

