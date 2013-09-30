package controllers.support
import org.scalatest._
import ticTacToe.CellState._

class BoardStateTest extends FunSpec with ShouldMatchers {

  it("should create sequence of cell states from a string") {
    BoardState.get("AXO") should be (Seq(Clear, X, O))
  }
  
  it("throws match error on invalid char") {
    intercept[MatchError] {
    	BoardState.get("CXO")
    }
  }
}