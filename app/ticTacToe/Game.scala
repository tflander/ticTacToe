package ticTacToe
import CellState._
import ticTacToe.ai.RandomAi
import ticTacToe.ai.SmarterAi
import ticTacToe.ai.SmartestAi

class Game {

  var board: Board = _
  val player1 = new SmarterAi(X)
  val player2 = new SmartestAi(O)
  //  val player2 = new SmarterAi(O)

  def play(): Board = {
    board = Board()
    while (true) {
      board = player1.takeSquare(board)
      printBoard()
      if (board.gameOver) return board
      board = player2.takeSquare(board)
      printBoard()
      if (board.gameOver) return board
    }
    return null
  }

  def printBoard() = {
    for (row <- 0 to 2) {
      for (col <- 0 to 2) {
        val mark = board.cellState(col, row)
        val icon = if (mark == Clear) "." else mark.toString
        print(icon)
      }
      println("")
    }
      println("")
  }
}
