package ticTacToe.ai.dsl
import org.scalatest._
import ticTacToe.CellState._
import ticTacToe.ai.rule.ProbableRule

class PrimaryRuleParserTest extends FunSpec with ShouldMatchers {

  class ConfigSpike(icon: CellState) extends PrimaryRuleParser {
    def iconFromClass = icon
    
    def parsePrimaryRule(string: String) = parseAll(primaryRule, string)
  }

  val configBuilder = new ConfigSpike(X)

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
    
    it("gives error message for invalid primary rule") {
      val p = configBuilder.parsePrimaryRule("is notValid")
      p.successful should be(false)
      p.toString should include("Member of Set(unbeatable, random)")
    }
  }
  
}