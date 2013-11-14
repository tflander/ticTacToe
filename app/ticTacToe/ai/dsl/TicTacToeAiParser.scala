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
import scala.annotation.tailrec

class TicTacToeAiParser(icon: CellState) extends OpeningRuleParser with PrimaryRuleParser with ExceptionRuleParser {

  def iconFromClass = icon

  def ruleSet: Parser[HumanizedAi] = opt(openingRule <~ ",") ~ primaryRule ~ opt("," ~> exceptionRules) ^^ {
    case openingRule ~ primaryRule ~ exceptionRule => BuildAi(openingRule, primaryRule, exceptionRule)
  }

  def BuildAi(openingRule: Option[AiRule], primaryRule: Seq[AiRule], exceptionRule: Option[List[AiRule]]) = {
    val exceptions = exceptionRule match {
      case Some(rules: List[AiRule]) => rules
      case None => Nil
    }
    new HumanizedAi(icon, openingRule, primaryRule, exceptions)
  }
}

trait OpeningRuleParser extends BaseRuleParser {

  val openingRules = Map(
    "randomly" -> new RandomRule(iconFromClass),
    "centerOrCorner" -> new CenterOrCorner(iconFromClass))

  def openingRule: Parser[AiRule] = "opens" ~ opt("with") ~> openingRuleNames ^^ (getRule(_, openingRules))
  def openingRuleNames: Parser[String] = buildStringParser(openingRules.keys) | ("Expected Member of " + openingRules.keys)
}

trait PrimaryRuleParser extends BaseRuleParser {

  def primaryRule: Parser[Seq[AiRule]] = primaryRuleDecorator ~> primaryRuleNames ^^ (getRule(_, primaryRules))
  def primaryRuleNames: Parser[String] = buildStringParser(primaryRules.keys) | ("Expected Member of " + primaryRules.keys)
  def primaryRuleDecorator: Parser[String] = "is" | "otherwise is" | ""

  val primaryRules: Map[String, Seq[AiRule]] = Map(
    "unbeatable" ->
      Seq(
        new Opener(iconFromClass),
        new Winner(iconFromClass),
        new Blocker(iconFromClass),
        new CornerNearOpponent(iconFromClass),
        new Priority(iconFromClass)),
    "random" ->
      Seq(new RandomRule(iconFromClass)))
}

trait ExceptionRuleParser extends BaseRuleParser {

  val simpleRules = Map(
    "win" -> new Winner(iconFromClass),
    "block" -> new Blocker(iconFromClass))

  val rulesToRemove = Map(
    "cornerNearOpponent" -> new ProbableRule(new CornerNearOpponent(iconFromClass), 0.0),
    "priority" -> new ProbableRule(new Priority(iconFromClass), 0.0))

  val probableRulesBaseRules = Map(
    "wins" -> new Winner(iconFromClass),
    "win" -> new Winner(iconFromClass),
    "blocks" -> new Blocker(iconFromClass))

  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"

  def probableException: Parser[ProbableRule] = opt("misses" | "plays") ~ probableRulesBaseRuleNames ~ probability ^^ { case qualifier ~ probableRule ~ probability => buildRule(qualifier, probableRule, probability) }
  def probableRulesBaseRuleNames: Parser[String] = buildStringParser(probableRulesBaseRules.keys) | ("Expected Member of " + probableRulesBaseRules.keys)

  def removeFromPrimaryRulesDecoratorBefore: Parser[String] = "misses the" | "except misses the"
  def removeFromPrimaryRulesDecoratorAfter: Parser[String] = "rule"
  def removeFromPrimaryRule: Parser[ProbableRule] = removeFromPrimaryRulesDecoratorBefore ~> removeFromPrimaryNames <~ removeFromPrimaryRulesDecoratorAfter ^^ (getRule(_, rulesToRemove))
  def removeFromPrimaryNames: Parser[String] = buildStringParser(rulesToRemove.keys) | ("Expected Member of " + rulesToRemove.keys)

  def simpleException: Parser[AiRule] = "never misses a " ~> simpleRuleName ^^ (getRule(_, simpleRules))
  def simpleRuleName: Parser[String] = buildStringParser(simpleRules.keys) | ("Expected Member of " + simpleRules.keys)

  def exception: Parser[AiRule] = simpleException | probableException | removeFromPrimaryRule
  def exceptionRule: Parser[AiRule] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[AiRule]] = repsep(exceptionRule, ",")

  def buildRule(qualifierOrNot: Option[String], probableRule: String, probability: Double) = {
    val triggerProbability = qualifierOrNot match {
      case Some(qualifier) => qualifier match {
        case "misses" => 1 - probability
        case _ => probability
      }
      case None => probability
    }
    
    probableRulesBaseRules.get(probableRule) match {
      case Some(rule) => new ProbableRule(rule, triggerProbability)
      case None => throw new IllegalArgumentException("unexpected rule " + probableRule)
    }
  }
}

trait BaseRuleParser extends JavaTokenParsers {

  def iconFromClass: CellState

  def buildStringParser(strings: Iterable[String]): Parser[String] = {
    buildStringParser(None, strings)
  }

  @tailrec final def buildStringParser(sParser: Option[Parser[String]], strings: Iterable[String]): Parser[String] = {
    val rule = strings.head
    val updatedParser: Parser[String] = sParser match {
      case Some(parser) => parser.append(rule)
      case None => rule
    }
    if (strings.tail.isEmpty) return updatedParser
    return buildStringParser(Some(updatedParser), strings.tail)
  }

  def getRule[T](rulex: Any, rules: Map[String, T]) = {
    val rule = rulex.toString
    rules.get(rule) match {
      case Some(ruleValue) => ruleValue
      case _ => throw new IllegalArgumentException("Expected Member of " + rules.keys + ", found: " + rule)
    }
  }
  
}
