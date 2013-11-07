package ticTacToe.ai
import org.scalatest._
import ticTacToe.ai.dsl.HumanizedConfigBuilder
import ticTacToe.ai.dsl._
import ticTacToe.CellState._
import scala.util.parsing.combinator.JavaTokenParsers
import ticTacToe.ai.rule.AiRule
import ticTacToe.CellState._
import ticTacToe.ai.rule.Winner
import ticTacToe.ai.rule.Blocker
import ticTacToe.ai.rule.Opener
import ticTacToe.ai.rule.CornerNearOpponent
import ticTacToe.ai.rule.Priority
import ticTacToe.ai.rule.RandomRule
import ticTacToe.ai.rule.CenterOrCorner

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
class ticTacToeAiParser(icon: CellState) extends JavaTokenParsers {
  def ai: Parser[Any] = "TBD"
  def probability: Parser[Double] = floatingPointNumber <~ probabilityDecorator ^^ (_.toDouble / 100)
  def probabilityDecorator: Parser[String] = "% of the time" | "%"
  def probableException: Parser[(AiRule, Double)] = probableRule ~ probability ^^ { case probableRule ~ probability => buildRule(probableRule, probability) }
  def probableRule: Parser[String] = "misses wins" | "misses blocks"
  def simpleException: Parser[AiRule] = simpleExceptionRule ^^ (buildRule(_))
  def simpleExceptionRule: Parser[String] = "never misses a win" | "never misses a block"
  def exception: Parser[Any] = simpleException | probableException
  def exceptionRule: Parser[Any] = exceptionDecorator ~> exception
  def exceptionDecorator: Parser[String] = "except" | "and" | ""
  def exceptionRules: Parser[List[Any]] = repsep(exceptionRule, ",")
  def primaryRuleName: Parser[String] = "random" | "unbeatable"
  def primaryRule: Parser[Seq[AiRule]] = primaryRuleName ^^ (buildPrimaryRule(_))
  def openingRule: Parser[AiRule] = openingRuleName ^^ (buildOpeningRule(_))
  def openingRuleName: Parser[String] = "randomly" | "with center or corner"

  def buildOpeningRule(rule: String) = {
    rule match {
      case "randomly" => new RandomRule(icon)
      case "with center or corner" => new CenterOrCorner(icon)
    }
  }
  
  def buildRule(probableRule: String, probability: Double) = {
    probableRule match {
      case "misses wins" => (new Winner(icon), probability)
      case "misses blocks" => (new Blocker(icon), probability)
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

class ConfigSpike(icon: CellState) extends ticTacToeAiParser(icon) {
  def buildAi(icon: CellState, string: String) = Nil
  def parseAi(string: String) = parseAll(ai, string)
  def parseProbability(string: String) = parseAll(probability, string)
  def parseProbableException(string: String) = parseAll(probableException, string)
  def parseSimpleException(string: String) = parseAll(simpleException, string)
  def parseException(string: String) = parseAll(exception, string)
  def parseExceptionRule(string: String) = parseAll(exceptionRule, string)
  def parseExceptionRules(string: String) = parseAll(exceptionRules, string)
  def parsePrimaryRule(string: String) = parseAll(primaryRule, string)
  def parseOpeningRule(string: String) = parseAll(openingRule, string)
}

class HumanizedConfigTest extends FunSpec with ShouldMatchers {

  val configBuilder = new ConfigSpike(X)
  
  describe("when openingRule") {
    
    it("can open randomly") {
      val p = configBuilder.parseOpeningRule("randomly")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("RandomRule")
    }
    
    it("can open strong") {
      val p = configBuilder.parseOpeningRule("with center or corner")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("CenterOrCorner")
    }
  }
  
  describe("when primary rule") {
    
    it("creates an unbeatable AI") {
      val p = configBuilder.parsePrimaryRule("unbeatable")
      p.successful should be(true)
      val rules = p.get.map(_.getClass.getSimpleName)
      rules should be(List("Opener", "Winner", "Blocker", "CornerNearOpponent", "Priority"))
    }
    
    it("creates a random AI") {
      val p = configBuilder.parsePrimaryRule("random")
      p.successful should be(true)
      val rules = p.get.map(_.getClass.getSimpleName)
      rules should be(List("RandomRule"))
    }
  }

  describe("when exception rules") {
    it("parses a single rule") {
      val p = configBuilder.parseExceptionRules("never misses a block")
      p.successful should be(true)
      p.get.head.getClass.getSimpleName should be("Blocker")
    }
    
    it("parses multiple rules") {
      val p = configBuilder.parseExceptionRules("never misses a block, and never misses a win")
      p.successful should be(true)
      val rules = p.get.map(_.getClass.getSimpleName)
      
      rules should be(List("Blocker", "Winner"))
    }
  }
  
  describe("when exception rule") {

    it("parses exception without additional decoration") {
      val p = configBuilder.parseExceptionRule("never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }
    
    it("allows the decorator 'and'") {
      val p = configBuilder.parseExceptionRule("and never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }
    
    it("allows the decorator 'except'") {
      val p = configBuilder.parseExceptionRule("except never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }
  }

  describe("when parse exception") {

    it("parses probable exceptions") {
      val p = configBuilder.parseException("misses wins 15% of the time")
      p.successful should be(true)
      p.get match {
        case ruleAndProbability: (Any, Any) => {
          ruleAndProbability._1.getClass.getSimpleName should be("Winner")
          ruleAndProbability._2 should be(0.15)
        }
      }
    }

    it("parses simple exceptions") {
      val p = configBuilder.parseException("never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }

  }

  describe("when parse simple exception") {
    it("should parse a flawless winner") {
      val p = configBuilder.parseSimpleException("never misses a win")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Winner")
    }

    it("should parse a flawless blocker") {
      val p = configBuilder.parseSimpleException("never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }

  }

  describe("when parse probable exception") {

    it("should parse the exception for winning") {
      val p = configBuilder.parseProbableException("misses wins 10% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Winner")
      p.get._2 should be(0.1)
    }

    it("should parse the exception for blocking") {
      val p = configBuilder.parseProbableException("misses blocks 10% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Blocker")
      p.get._2 should be(0.1)
    }

  }

  describe("when parse probability") {

    it("should parse the short form") {
      val p = configBuilder.parseProbability("10%")
      p.successful should be(true)
      p.get should be(0.1)
    }

    it("should parse the long form") {
      val p = configBuilder.parseProbability("20% of the time")
      p.successful should be(true)
      p.get should be(0.2)
    }

  }

  describe("Humanized Config Tests") {

    it("should create an unbeatable AI") {
      val aiRules = configBuilder.buildAi(X, "is unbeatable");
      // TODO:  validation
    }

    it("should create an AI that opens randomly") {
      val aiRules = configBuilder.buildAi(X, "opens randomly, otherwise is unbeatable");
      // TODO:  validation
    }

    it("should create an AI that opens with center or corner, then moves randomly") {
      val aiRules = configBuilder.buildAi(X, "opens with center or corner, otherwise is random");
      // TODO:  validation
    }

    it("should create an AI that opens with center or corner, then moves randomly, misses blocks 10% of the time") {
      val aiRules = configBuilder.buildAi(X, "opens with center or corner, otherwise is random, blocks 90% of the time, never misses a win");
      // TODO:  validation
    }

    it("same as above, but can use 'and' to chain exception rules") {
      val aiRules = configBuilder.buildAi(X, "opens with center or corner, otherwise is random, blocks 90% of the time, and never misses a win");
      // TODO:  validation
    }

    it("defensive ai using whitespace to stack vertically") {
      val aiRules = configBuilder.buildAi(X, """
          opens with center or corner, 
    		  otherwise is random, 
    		  plays win 90% of the time, 
    		  and never misses a block
          """);
      // TODO:  validation
    }

    it("should create an AI that opens randomly, plays strong, but sometimes misses a block") {
      val aiRules = configBuilder.buildAi(X, "opens randomly, otherwise is unbeatable, except misses blocks 10% of the time");
      // TODO:  validation
    }

    it("should create an AI that is generally unbeatable, but sometimes misses a win") {
      val aiRules = configBuilder.buildAi(X, "is unbeatable, except misses wins 10% of the time");
      // TODO:  validation
    }

    it("can short-hand an exception") {
      val aiRules = configBuilder.buildAi(X, "is unbeatable, except misses wins 10%");
      // TODO:  validation      
    }

  }
}
