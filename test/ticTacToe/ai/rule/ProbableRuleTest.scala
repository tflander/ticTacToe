package ticTacToe.ai.rule
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class ProbableRuleTest extends FunSpec with ShouldMatchers {

  object FakeRule extends AiRule {
    def squareToPlay(board: Board): Option[(Int, Int)] = Some(0,0)
  }
  
  val rule = new ProbableRule(FakeRule, 0.5)
  val emptyBoard = Board()
  
  it("should envoke the rule half the time according to the probability") {
    val results = for (i <- 1 to 2000) yield rule.squareToPlay(emptyBoard)
    results.count(_ == None) should (be >= 900 and be <= 1100)
  }
}
