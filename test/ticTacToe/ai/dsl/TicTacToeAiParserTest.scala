package ticTacToe.ai.dsl
import org.scalatest._
import ticTacToe.CellState._

class TicTacToeAiParserTest extends FunSpec with ShouldMatchers {

  class ConfigSpike(icon: CellState) extends TicTacToeAiParser(icon) {
    //  def parseAi(string: String) = parseAll(ai, string)
    def parseProbability(string: String) = parseAll(probability, string)
    def parseProbableException(string: String) = parseAll(probableException, string)
    def parseSimpleException(string: String) = parseAll(simpleException, string)
    def parseException(string: String) = parseAll(exception, string)
    def parseExceptionRule(string: String) = parseAll(exceptionRule, string)
    def parseExceptionRules(string: String) = parseAll(exceptionRules, string)
    def parsePrimaryRule(string: String) = parseAll(primaryRule, string)
    def parseOpeningRule(string: String) = parseAll(openingRule, string)
  }

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

    it("supports decorator 'opens'") {
      val p = configBuilder.parseOpeningRule("opens with center or corner")
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

    it("supports decoration with 'is'") {
      val p = configBuilder.parsePrimaryRule("is random")
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
          ruleAndProbability._2 should be(0.85)
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

    it("allows specifying the blocking exception") {
      val p = configBuilder.parseProbableException("blocks 90% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Blocker")
      p.get._2 should be(0.9)
    }

    it("allows specifying the winning exception") {
      val p = configBuilder.parseProbableException("wins 90% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Winner")
      p.get._2 should be(0.9)
    }

    it("should parse the exception for winning") {
      val p = configBuilder.parseProbableException("misses wins 10% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Winner")
      p.get._2 should be(0.9)
    }

    it("should parse the exception for blocking") {
      val p = configBuilder.parseProbableException("misses blocks 10% of the time")
      p.successful should be(true)
      p.get._1.getClass.getSimpleName should be("Blocker")
      p.get._2 should be(0.9)
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
  
}