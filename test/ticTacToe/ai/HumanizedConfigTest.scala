package ticTacToe.ai
import org.scalatest._
import ticTacToe.ai.dsl._
import ticTacToe.CellState._
import scala.util.parsing.combinator.JavaTokenParsers
import ticTacToe.ai.rule.AiRule
import ticTacToe.CellState._
import ticTacToe.ai.rule.ProbableRule

class ConfigSpike(icon: CellState) extends TicTacToeAiParser(icon) {
  def buildAi(string: String) = parseAll(ruleSet, string)
}

class HumanizedConfigTest extends FunSpec with ShouldMatchers {

  val configBuilder = new ConfigSpike(X)
  val unbeatableRules = "List(Opener(X), Winner(X), Blocker(X), CornerNearOpponent(X), Priority(X))"
  
  describe("Humanized Config Tests") {
    
    it("should create an unbeatable AI") {
      val aiRules = configBuilder.buildAi("is unbeatable");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRules.toString should be (unbeatableRules)
      ai.exceptionRules should be(Nil)
    }
    
    it("should create an AI that doesn't know the corner near opponent rule, otherwise is unbeatable") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses the cornerNearOpponent rule");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRulesExceptionsRemoved.toString should be ("List(Opener(X), Winner(X), Blocker(X), Priority(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(CornerNearOpponent(X),0.0))")
    }
    
    it("should create an AI that doesn't know the priority rule, otherwise is unbeatable") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses the priority rule");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRulesExceptionsRemoved.toString should be ("List(Opener(X), Winner(X), Blocker(X), CornerNearOpponent(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Priority(X),0.0))")
    }

    it("should create an AI that opens randomly") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.get.toString should be("RandomRule(X)")
      ai.primaryRules.toString should be ("List(Opener(X), Winner(X), Blocker(X), CornerNearOpponent(X), Priority(X))")
      ai.exceptionRules should be(Nil)
    }

    it("should create an AI that opens with center or corner, then moves randomly") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.get.toString should be("CenterOrCorner(X)")
      ai.primaryRules.toString should be ("List(RandomRule(X))")
      ai.exceptionRules should be(Nil)
    }

    it("should create an AI that opens with center or corner, then moves randomly, misses blocks 10% of the time") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random, blocks 90% of the time, never misses a win");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.get.toString should be("CenterOrCorner(X)")
      ai.primaryRules.toString should be ("List(RandomRule(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Blocker(X),0.9), Winner(X))")
    }

    it("same as above, but can use 'and' to chain exception rules") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random, blocks 90% of the time, and never misses a win");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.get.toString should be("CenterOrCorner(X)")
      ai.primaryRules.toString should be ("List(RandomRule(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Blocker(X),0.9), Winner(X))")
    }

    it("supports using whitespace to stack vertically") {
      val aiRules = configBuilder.buildAi("""
          opens with centerOrCorner, 
    		  otherwise is random, 
    		  plays win 90% of the time, 
    		  and never misses a block
          """);
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.toString should be("Some(CenterOrCorner(X))")
      ai.primaryRules.toString should be ("List(RandomRule(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Winner(X),0.9), Blocker(X))")
    }

    it("should create an AI that opens randomly, plays strong, but sometimes misses a block") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable, except misses blocks 10% of the time");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule.toString should be("Some(RandomRule(X))")
      ai.primaryRulesExceptionsRemoved.toString should be ("List(Opener(X), Winner(X), CornerNearOpponent(X), Priority(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Blocker(X),0.9))")
    }

    it("should create an AI that is generally unbeatable, but sometimes misses a win") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses wins 10% of the time");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRulesExceptionsRemoved.toString should be ("List(Opener(X), Blocker(X), CornerNearOpponent(X), Priority(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Winner(X),0.9))")
    }

    it("can short-hand an exception") {
      val aiRules = configBuilder.buildAi("is unbeatable, misses wins 10%");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRulesExceptionsRemoved.toString should be ("List(Opener(X), Blocker(X), CornerNearOpponent(X), Priority(X))")
      ai.exceptionRules.toString should be("List(ProbableRule(Winner(X),0.9))")
    }

  }
}
