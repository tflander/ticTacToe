package ticTacToe
import CellState._
import ticTacToe.ai.RandomAi
import ticTacToe.ai.SmarterAi
import ticTacToe.ai.SmartestAi
import ticTacToe.ai.ComputerPlayer

class Game {

  var board: Board = _

  def play(x: ComputerPlayer, o: ComputerPlayer): Board = {
    board = Board()
    while (true) {
      board = x.takeSquare(board)
      printBoard()
      if (board.gameOver) return board
      board = o.takeSquare(board)
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
