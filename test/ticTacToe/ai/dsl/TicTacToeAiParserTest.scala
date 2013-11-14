package ticTacToe.ai.dsl
import org.scalatest._
import ticTacToe.CellState._
import ticTacToe.ai.rule.ProbableRule

class TicTacToeAiParserTest extends FunSpec with ShouldMatchers {

  class ConfigSpike(icon: CellState) extends TicTacToeAiParser(icon) {

    def parseProbability(string: String) = parseAll(probability, string)
    def parseProbableException(string: String) = parseAll(probableException, string)
    def parseSimpleException(string: String) = parseAll(simpleException, string)

    def parseException(string: String) = parseAll(exception, string)
    def parseExceptionRule(string: String) = parseAll(exceptionRule, string)
    def parseExceptionRules(string: String) = parseAll(exceptionRules, string)

    def parseRemovePrimaryRule(string: String) = parseAll(removeFromPrimaryRule, string)
  }

  val configBuilder = new ConfigSpike(X)

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

    it("allows the decorator 'except' with a simple rule") {
      val p = configBuilder.parseExceptionRule("except never misses a block")
      p.successful should be(true)
      p.get.getClass.getSimpleName should be("Blocker")
    }

    it("allows the decorator 'except' with a probability rule") {
      val p = configBuilder.parseExceptionRule("except misses blocks 10% of the time")
      p.successful should be(true)

      p.get match {
        case rule: ProbableRule => {
          rule.baseRule.getClass.getSimpleName should be("Blocker")
          rule.probability should be(0.90)
        }
      }
    }
  }

  describe("removing a primary rule") {

    it("can remove the corner near opponent rule") {
      val p = configBuilder.parseRemovePrimaryRule("misses the cornerNearOpponent rule")
      p.successful should be(true)

      p.get match {
        case rule: ProbableRule => {
          rule.baseRule.getClass.getSimpleName should be("CornerNearOpponent")
          rule.probability should be(0.0)
        }
      }
    }

    it("can remove the corner near opponent rule using 'except' decorator") {
      val p = configBuilder.parseRemovePrimaryRule("except misses the cornerNearOpponent rule")
      println(p)
      p.successful should be(true)

      p.get match {
        case rule: ProbableRule => {
          rule.baseRule.getClass.getSimpleName should be("CornerNearOpponent")
          rule.probability should be(0.0)
        }
      }
    }

    it("can remove the priority rule") {
      val p = configBuilder.parseRemovePrimaryRule("misses the priority rule")
      p.successful should be(true)

      p.get match {
        case rule: ProbableRule => {
          rule.baseRule.getClass.getSimpleName should be("Priority")
          rule.probability should be(0.0)
        }
      }
    }

    it("gives error message for invalid rule to remove") {
      val p = configBuilder.parseRemovePrimaryRule("misses the invalidRule rule")
      p.successful should be(false)
      p.toString should include("Member of Set(cornerNearOpponent, priority)")
    }
  }

  describe("when parse exception") {

    it("parses probable exceptions") {
      val p = configBuilder.parseException("misses wins 15% of the time")
      p.successful should be(true)
      p.get match {
        case rule: ProbableRule => {
          rule.baseRule.getClass.getSimpleName should be("Winner")
          rule.probability should be(0.85)
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

    it("should fail an invalid rule") {
      val p = configBuilder.parseSimpleException("never misses a meal")
      p.toString should include("Member of Set(win, block)")
    }
  }

  describe("when parse probable exception") {

    it("allows specifying the blocking exception") {
      val p = configBuilder.parseProbableException("blocks 90% of the time")
      p.successful should be(true)
      p.get.baseRule.getClass.getSimpleName should be("Blocker")
      p.get.probability should be(0.9)
    }

    it("allows specifying the winning exception") {
      val p = configBuilder.parseProbableException("wins 90% of the time")
      p.successful should be(true)
      p.get.baseRule.getClass.getSimpleName should be("Winner")
      p.get.probability should be(0.9)
    }

    it("should parse the exception for winning") {
      val p = configBuilder.parseProbableException("misses wins 10% of the time")
      p.successful should be(true)
      p.get.baseRule.getClass.getSimpleName should be("Winner")
      p.get.probability should be(0.9)
    }

    it("should parse the exception for blocking") {
      val p = configBuilder.parseProbableException("misses blocks 10% of the time")
      p.successful should be(true)
      p.get.baseRule.getClass.getSimpleName should be("Blocker")
      p.get.probability should be(0.9)
    }

    it("supports alternative syntax for winning") {
      val p = configBuilder.parseProbableException("plays win 90% of the time")
      p.successful should be(true)
      p.get.baseRule.getClass.getSimpleName should be("Winner")
      p.get.probability should be(0.9)
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