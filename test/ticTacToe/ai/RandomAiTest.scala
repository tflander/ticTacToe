package ticTacToe.ai
import org.scalatest._
import ticTacToe.Board
import ticTacToe.CellState._

class RandomAiTest extends FunSpec with ShouldMatchers {

  it("should place a random X on a new board") {
    val randomAi = new RandomAi(X)
    val boardWithX = randomAi.takeSquare(Board())
    boardWithX.turnsPlayed should be(1)
  }
  
  it("should fill the board given 9 consecutive plays") {
    val randomAi = new RandomAi(X)
    val move1 = randomAi.takeSquare(Board())
    val move2 = randomAi.takeSquare(move1)
    val move3 = randomAi.takeSquare(move2)
    val move4 = randomAi.takeSquare(move3)
    val move5 = randomAi.takeSquare(move4)
    val move6 = randomAi.takeSquare(move5)
    val move7 = randomAi.takeSquare(move6)
    val move8 = randomAi.takeSquare(move7)
    val move9 = randomAi.takeSquare(move8)
    move1.turnsPlayed should be(1)
    move2.turnsPlayed should be(2)
    move3.turnsPlayed should be(3)
    move4.turnsPlayed should be(4)
    move5.turnsPlayed should be(5)
    move6.turnsPlayed should be(6)
    move7.turnsPlayed should be(7)
    move8.turnsPlayed should be(8)
    move9.turnsPlayed should be(9)
  }
  
  it("won't allow a move on a full board") {
    val randomAi = new RandomAi(X)
    val board = Board()
    val move1 = randomAi.takeSquare(Board())
    val move2 = randomAi.takeSquare(move1)
    val move3 = randomAi.takeSquare(move2)
    val move4 = randomAi.takeSquare(move3)
    val move5 = randomAi.takeSquare(move4)
    val move6 = randomAi.takeSquare(move5)
    val move7 = randomAi.takeSquare(move6)
    val move8 = randomAi.takeSquare(move7)
    val move9 = randomAi.takeSquare(move8)
    
    intercept[IllegalArgumentException] {
      randomAi.takeSquare(move9)
    }
  }
}