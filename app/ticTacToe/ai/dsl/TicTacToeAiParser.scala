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
  // TODO:  These should be dynamically defined
  def probableRule: Parser[String] = "misses wins" | "misses blocks" | "wins" | "blocks" | "plays win"
  def simpleExceptionRule: Parser[String] = "never misses a win" | "never misses a block"
//  def openingRuleName: Parser[String] = "randomly" | "with center or corner"
  // End of TODO for things that need to be dynamically defined

  def ruleSet: Parser[HumanizedAi] = opt(openingRule <~ ",") ~ primaryRule ~ opt("," ~> exceptionRules) ^^ {
    case openingRule ~ primaryRule ~ exceptionRule => BuildAi(openingRule, primaryRule, exceptionRule)
  }

  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"

  def probableException: Parser[ProbableRule] = probableRule ~ probability ^^ { case probableRule ~ probability => buildRule(probableRule, probability) }

  def removeFromPrimaryRulesDecoratorBefore: Parser[String] = "misses the" | "except misses the"
  def removeFromPrimaryRulesDecoratorAfter: Parser[String] = "rule"
  def removeFromPrimaryRule: Parser[ProbableRule] = removeFromPrimaryRulesDecoratorBefore ~> ident <~ removeFromPrimaryRulesDecoratorAfter ^^ (buildRuleToRemove(_))

  def simpleException: Parser[AiRule] = simpleExceptionRule ^^ (buildRule(_))

  def exception: Parser[AiRule] = simpleException | probableException | removeFromPrimaryRule
  def exceptionRule: Parser[AiRule] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[AiRule]] = repsep(exceptionRule, ",")

  def primaryRule: Parser[Seq[AiRule]] = primaryRuleDecorator ~> ident ^^ (buildPrimaryRule(_))
  def primaryRuleDecorator: Parser[String] = "is" | "otherwise is" | ""

  def openingRule: Parser[AiRule] = openingDecorator ~ opt("with") ~> ident ^^ (buildOpeningRule(_))
  def openingDecorator: Parser[String] = "opens" 

  def BuildAi(openingRule: Option[AiRule], primaryRule: Seq[AiRule], exceptionRule: Option[List[AiRule]]) = {
    val exceptions = exceptionRule match {
      case Some(rules: List[AiRule]) => rules
      case None => Nil
    }
    new HumanizedAi(icon, openingRule, primaryRule, exceptions)
  }
  
  val rulesToRemove = Map(
	"cornerNearOpponent" -> new ProbableRule(new CornerNearOpponent(icon), 0.0),
	"priority" -> new ProbableRule(new Priority(icon), 0.0)
  )

  def buildRuleToRemove(rule: String) = {
    rulesToRemove.get(rule) match {
      case Some(ruleToRemove) => ruleToRemove
      case _ => throw new IllegalArgumentException("Expected Member of " + rulesToRemove.keys + ", found: " + rule)
    }
  }

  val openingRules = Map(
      "randomly" -> new RandomRule(icon),
      "centerOrCorner" -> new CenterOrCorner(icon)      
  )
  
  def buildOpeningRule(rule: String) = {
    openingRules.get(rule) match {
      case Some(openingRule) => openingRule
      case _ => throw new IllegalArgumentException("Expected Member of " + openingRules.keys + ", found: " + rule)
    }
  }

  def buildRule(probableRule: String, probability: Double) = {
    probableRule match {
      case "misses wins" => new ProbableRule(new Winner(icon), 1 - probability)
      case "misses blocks" => new ProbableRule(new Blocker(icon), 1 - probability)
      case "wins" => new ProbableRule(new Winner(icon), probability)
      case "plays win" => new ProbableRule(new Winner(icon), probability)
      case "blocks" => new ProbableRule(new Blocker(icon), probability)
    }
  }

  val primaryRules: Map[String, Seq[AiRule]] = Map(
    "unbeatable" ->
      Seq(
        new Opener(icon),
        new Winner(icon),
        new Blocker(icon),
        new CornerNearOpponent(icon),
        new Priority(icon)),
    "random" ->
      Seq(new RandomRule(icon)))

  def buildPrimaryRule(rule: String) = {
    primaryRules.get(rule) match {
      case Some(ruleSeq) => ruleSeq
      case None => throw new IllegalArgumentException("Expected Member of " + primaryRules.keys + ", found: " + rule)
    }    
  }

  def buildRule(rule: String): AiRule = {
    rule match {
      case "never misses a win" => new Winner(icon)
      case "never misses a block" => new Blocker(icon)
    }
  }
}

