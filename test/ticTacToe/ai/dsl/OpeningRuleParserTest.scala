package ticTacToe.ai.dsl
import org.scalatest._
import ticTacToe.CellState._
import ticTacToe.ai.rule.ProbableRule

class OpeningRuleParserTest extends FunSpec with ShouldMatchers {

  class ConfigSpike(icon: CellState) extends OpeningRuleParser {
    def iconFromClass: CellState = icon
    def parseOpeningRule(string: String) = parseAll(openingRule, string)
  }

  val configBuilder = new ConfigSpike(X)

  it("can open randomly") {
    val p = configBuilder.parseOpeningRule("opens randomly")
    p.successful should be(true)
    p.get.getClass.getSimpleName should be("RandomRule")
  }

  it("can open strong using optional 'with' decorator") {
    val p = configBuilder.parseOpeningRule("opens with centerOrCorner")
    p.successful should be(true)
    p.get.getClass.getSimpleName should be("CenterOrCorner")
  }

  it("gives error for invalid opening rule") {
    val p = configBuilder.parseOpeningRule("opens usingInvalidRule")
    p.successful should be(false)
    p.toString should include("Member of Set(randomly, centerOrCorner)' expected")
  }

}