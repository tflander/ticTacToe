package ticTacToe.ai.rule
import ticTacToe.Board
import scala.util.Random
import java.util.Date
import ticTacToe.CellState._

trait AiRule {
  def squareToPlay(board: Board): Option[(Int, Int)]

  val random = new Random(new Date().getTime)

  def takeCorner(): Option[(Int, Int)] = {
    return random.nextInt(4) match {
      case 0 => Some(0, 0)
      case 1 => Some(0, 2)
      case 2 => Some(2, 0)
      case 3 => Some(2, 2)
    }
  }

  def takeCenter(): Option[(Int, Int)] = Some(1, 1)

  def canWinThisTurn(board: Board, icon: CellState)(cells: Seq[CellState]): Boolean = {
    val iHaveAllButOne = board.boardSizeMinusOne == cells.count(_ == icon)
    val oneIsClear = 1 == cells.count(_ == Clear)
    return iHaveAllButOne && oneIsClear
  }
  
      def winningPositionOnBoardForCellSequence(board: Board)(cells: Seq[CellState]): Int = {
      for (i <- 0 to board.boardSizeMinusOne) {
        if (cells(i) == Clear) return i
      }
      throw new IllegalArgumentException("no clear cell available")
    }


}