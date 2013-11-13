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
  val unbeatableRules = Seq("Opener", "Winner", "Blocker", "CornerNearOpponent", "Priority")
  
  describe("Humanized Config Tests") {
    
    def namesOf(rules: Seq[AiRule]) = rules map(nameOf)
    def nameOf(rule: AiRule) = {
      rule match {
        case pRule: ProbableRule => {
          pRule.getClass.getSimpleName + "(" + pRule.baseRule.getClass.getSimpleName + ", " + pRule.probability + ")"
        }
        case _ => rule.getClass.getSimpleName
      }
    }

    it("should create an unbeatable AI") {
      val aiRules = configBuilder.buildAi("is unbeatable");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      namesOf(ai.primaryRules) should be (unbeatableRules)
      ai.exceptionRules should be(Nil)
    }
    
    it("should create an AI that doesn't know the corner near opponent rule, otherwise is unbeatable") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses the cornerNearOpponent rule");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      namesOf(ai.primaryRulesExceptionsRemoved) should be (List("Opener", "Winner", "Blocker", "Priority"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(CornerNearOpponent, 0.0)"))
    }
    
    it("should create an AI that doesn't know the priority rule, otherwise is unbeatable") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses the priority rule");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      namesOf(ai.primaryRulesExceptionsRemoved) should be (List("Opener", "Winner", "Blocker", "CornerNearOpponent"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Priority, 0.0)"))
    }

    it("should create an AI that opens randomly") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      nameOf(ai.openingRule.get) should be("RandomRule")
      namesOf(ai.primaryRules) should be (Seq("Opener", "Winner", "Blocker", "CornerNearOpponent", "Priority"))
      ai.exceptionRules should be(Nil)
    }

    it("should create an AI that opens with center or corner, then moves randomly") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      nameOf(ai.openingRule.get) should be("CenterOrCorner")
      namesOf(ai.primaryRules) should be (Seq("RandomRule"))
      ai.exceptionRules should be(Nil)
    }

    it("should create an AI that opens with center or corner, then moves randomly, misses blocks 10% of the time") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random, blocks 90% of the time, never misses a win");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      nameOf(ai.openingRule.get) should be("CenterOrCorner")
      namesOf(ai.primaryRules) should be (Seq("RandomRule"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Blocker, 0.9)", "Winner"))
    }

    it("same as above, but can use 'and' to chain exception rules") {
      val aiRules = configBuilder.buildAi("opens with centerOrCorner, otherwise is random, blocks 90% of the time, and never misses a win");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      nameOf(ai.openingRule.get) should be("CenterOrCorner")
      namesOf(ai.primaryRules) should be (Seq("RandomRule"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Blocker, 0.9)", "Winner"))
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
      nameOf(ai.openingRule.get) should be("CenterOrCorner")
      namesOf(ai.primaryRules) should be (Seq("RandomRule"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Winner, 0.9)", "Blocker"))
    }

    it("should create an AI that opens randomly, plays strong, but sometimes misses a block") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable, except misses blocks 10% of the time");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      nameOf(ai.openingRule.get) should be("RandomRule")
      namesOf(ai.primaryRulesExceptionsRemoved) should be (Seq("Opener", "Winner", "CornerNearOpponent", "Priority"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Blocker, 0.9)"))
    }

    it("should create an AI that is generally unbeatable, but sometimes misses a win") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses wins 10% of the time");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      namesOf(ai.primaryRulesExceptionsRemoved) should be (Seq("Opener", "Blocker", "CornerNearOpponent", "Priority"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Winner, 0.9)"))
    }

    it("can short-hand an exception") {
      val aiRules = configBuilder.buildAi("is unbeatable, misses wins 10%");
      aiRules.successful should be(true)
      val ai = aiRules.get
      ai.icon should be(X)
      ai.openingRule should be(None)
      namesOf(ai.primaryRulesExceptionsRemoved) should be (Seq("Opener", "Blocker", "CornerNearOpponent", "Priority"))
      namesOf(ai.exceptionRules) should be(Seq("ProbableRule(Winner, 0.9)"))
    }

  }
}
