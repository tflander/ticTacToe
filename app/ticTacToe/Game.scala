package ticTacToe
import CellState._
import ticTacToe.ai.RandomAi
import ticTacToe.ai.SmarterAi
import ticTacToe.ai.SmartestAi

class Game {

  var board: Board = _
  val player1 = new SmartestAi(X)
//  val player2 = new SmartestAi(O)
  val player2 = new SmarterAi(O)
  
  def play(): Board = {
    board = Board()
    while (true) {
      board = player1.takeSquare(board)
      if(board.gameOver) return board
      board = player2.takeSquare(board)
      if(board.gameOver) return board
    }
    return null
  }
}
