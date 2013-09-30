package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

class Opener(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    board.turnsPlayed match {
      case 0 => return opener()
      case 1 => return firstResponse(board)
      case 2 => return thirdMove(board)
      case _ => return None
    }
  }

  def opener(): Option[(Int, Int)] = {
    return random.nextInt(5) match {
      case 0 => takeCenter()
      case _ => takeCorner() 
    }
  }
  
  def firstResponse(board: Board): Option[(Int, Int)] = {
    val opponentsMove = board.occupiedSquares(opponent).head
    if(opponentsMove == (1, 1)) takeCorner() else takeCenter()
  }

  def thirdMove(board: Board): Option[(Int, Int)] = {
    val myMove = board.occupiedSquares(icon).head
    val opponentsMove = board.occupiedSquares(opponent).head
    
    if(myMove == (1, 1)) {
      opponentsMove match {
        case (0, 0) => return Some((2, 2))
        case (2, 0) => return Some((0, 2))
        case (0, 2) => return Some((2, 0))
        case (2, 2) => return Some((0, 0))
        case _ => return None
      }
    }
    
    if(opponentsMove == (1, 1)) {
      myMove match {
        case (0, 0) => return Some((0, 2))
        case (2, 0) => return Some((0, 0))
        case (0, 2) => return Some((0, 0))
        case (2, 2) => return Some((0, 2))
        case _ => return None
      }
    }
    
    if(opponentsMove._1 == 1 || opponentsMove._2 == 1) {
      opponentsMove match {
        case (0, 1) => return if (board.cellState(0, 0) == Clear) Some((0,0)) else Some((0,2))
        case (2, 1) => return if (board.cellState(2, 0) == Clear) Some((2,0)) else Some((2,2))
        case (1, 0) => return if (board.cellState(0, 0) == Clear) Some((0,0)) else Some((2,0))
        case (1, 2) => return if (board.cellState(2, 2) == Clear) Some((2,2)) else Some((0,2))
      }
    }
    
    None
  }
}