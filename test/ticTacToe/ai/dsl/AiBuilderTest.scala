package ticTacToe.ai.dsl
import org.scalatest._
import ticTacToe.CellState._
import AiBuilder._

class AiBuilderTest extends FunSpec with ShouldMatchers {

  it("should create AI for any valid expression as tested in HumanizedConfigTest") {
    val ai = buildAi(X, "is unbeatable")
    println(ai)
      ai.icon should be(X)
      ai.openingRule should be(None)
      ai.primaryRules.size should be (5)
      ai.exceptionRules should be(Nil)
  }
  
  it("should handle a parsing error") {
    val e = intercept[IllegalArgumentException] {
    	buildAi(X, "is sorta unbeatable")      
    }
    e.getMessage() should include ("Member of Set(unbeatable, random)")
  }
}