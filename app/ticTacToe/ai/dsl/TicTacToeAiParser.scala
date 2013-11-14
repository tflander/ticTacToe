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


trait ExceptionRuleParser extends BaseRuleParser {

  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"

  def probableException: Parser[ProbableRule] = opt("misses" | "plays") ~ ident ~ probability ^^ { case qualifier ~ probableRule ~ probability => buildRule(qualifier, probableRule, probability) }

  def removeFromPrimaryRulesDecoratorBefore: Parser[String] = "misses the" | "except misses the"
  def removeFromPrimaryRulesDecoratorAfter: Parser[String] = "rule"
  def removeFromPrimaryRule: Parser[ProbableRule] = removeFromPrimaryRulesDecoratorBefore ~> ident <~ removeFromPrimaryRulesDecoratorAfter ^^ (getRule(_, rulesToRemove))

  def simpleException: Parser[AiRule] = "never misses a " ~> ident ^^ (getRule(_, simpleRules))

  def exception: Parser[AiRule] = simpleException | probableException | removeFromPrimaryRule
  def exceptionRule: Parser[AiRule] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[AiRule]] = repsep(exceptionRule, ",")

  val rulesToRemove = Map(
    "cornerNearOpponent" -> new ProbableRule(new CornerNearOpponent(iconFromClass), 0.0),
    "priority" -> new ProbableRule(new Priority(iconFromClass), 0.0))

  def buildRule(qualifierOrNot: Option[String], probableRule: String, probability: Double) = {
    val triggerProbability = qualifierOrNot match {
      case Some(qualifier) => qualifier match {
        case "misses" => 1 - probability
        case _ => probability
      }
      case None => probability
    }

    probableRule match {
      case "wins" => new ProbableRule(new Winner(iconFromClass), triggerProbability)
      case "win" => new ProbableRule(new Winner(iconFromClass), triggerProbability)
      case "blocks" => new ProbableRule(new Blocker(iconFromClass), triggerProbability)
    }
  }

  val simpleRules = Map(
    "win" -> new Winner(iconFromClass),
    "block" -> new Blocker(iconFromClass))
}

trait OpeningRuleParser extends BaseRuleParser {

  val openingRules = Map(
		  "randomly" -> new RandomRule(iconFromClass),
		  "centerOrCorner" -> new CenterOrCorner(iconFromClass))
		  
  def openingRule: Parser[AiRule] = "opens" ~ opt("with") ~> openingRuleNames ^^ (getRule(_, openingRules))
  def openingRuleNames: Parser[String] =  buildStringParser(None, openingRules.keys) | ("Expected Member of " + openingRules.keys)
}

trait PrimaryRuleParser extends BaseRuleParser {

  def primaryRule: Parser[Seq[AiRule]] = primaryRuleDecorator ~> ident ^^ (getRule(_, primaryRules))
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

trait BaseRuleParser extends JavaTokenParsers {
  
  def iconFromClass: CellState
  
  @tailrec final def buildStringParser(sParser: Option[Parser[String]], strings: Iterable[String]): Parser[String] = {
    val rule = strings.head
    val updatedParser: Parser[String] = sParser match {
      case Some(parser) => parser.append(rule)
      case None => rule
    }
    if(strings.tail.isEmpty) return updatedParser
    return buildStringParser(Some(updatedParser), strings.tail)
  }
  
  // TODO: DRY
  def getRule(rulex: Any, rules: Map[String, AiRule]) = {
    val rule = rulex.toString
    rules.get(rule) match {
      case Some(openingRule) => openingRule
      case _ => throw new IllegalArgumentException("Expected Member of " + rules.keys + ", found: " + rule)
    }
  }
  
  // TODO: DRY
  def getRule(rulex: Any, rules: Map[String, ProbableRule]) = {
    val rule = rulex.toString
    rules.get(rule) match {
      case Some(openingRule) => openingRule
      case _ => throw new IllegalArgumentException("Expected Member of " + rules.keys + ", found: " + rule)
    }
  }
  
  // TODO:  DRY
  def getRule(rulex: Any, rules: Map[String, Seq[AiRule]]) = {
    val rule = rulex.toString
    rules.get(rule) match {
      case Some(openingRule) => openingRule
      case _ => throw new IllegalArgumentException("Expected Member of " + rules.keys + ", found: " + rule)
    }
  }
  
}
