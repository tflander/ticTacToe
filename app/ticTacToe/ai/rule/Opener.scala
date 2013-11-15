package ticTacToe.ai.rule
import ticTacToe.CellState._
import ticTacToe.Board

case class Opener(icon: CellState) extends AiRule {

  val opponent = if (icon == X) O else X

  override def squareToPlay(board: Board): Option[(Int, Int)] = {
    board.turnsPlayed match {
      case 0 => return firstMove()
      case 1 => return secondMove(board)
      case 2 => return thirdMove(board)
      case 3 => return fourthMove(board)
      case _ => return None
    }
  }

  def firstMove(): Option[(Int, Int)] = {
    return random.nextInt(5) match {
      case 0 => takeCenter()
      case _ => takeCorner()
    }
  }

  def secondMove(board: Board): Option[(Int, Int)] = {
    val opponentsMove = board.occupiedSquares(opponent).head
    if (opponentsMove == (1, 1)) takeCorner() else takeCenter()
  }

  def thirdMove(board: Board): Option[(Int, Int)] = {
    val myMove = board.occupiedSquares(icon).head
    val opponentsMove = board.occupiedSquares(opponent).head
    
    def takeOppositeCornerIfHaveCenterAndOpponentTookCorner(): Option[(Int, Int)] = {
      if (myMove == (1, 1)) {
        opponentsMove match {
          case (0, 0) => return Some((2, 2))
          case (2, 0) => return Some((0, 2))
          case (0, 2) => return Some((2, 0))
          case (2, 2) => return Some((0, 0))
          case _ => return None
        }
      }
      return None
    }

    def takeAdjacentCornerIfHaveCornerAndOpponentTookCenter(): Option[(Int, Int)] = {
      if (opponentsMove == (1, 1)) {
        myMove match {
          case (0, 0) => return Some((0, 2))
          case (2, 0) => return Some((0, 0))
          case (0, 2) => return Some((0, 0))
          case (2, 2) => return Some((0, 2))
          case _ => return None
        }
      }
      return None
    }

    def takeAdjacentCornerIfOpponentTookEdge(): Option[(Int, Int)] = {
      if (opponentsMove._1 != opponentsMove._2 && (opponentsMove._1 == 1 || opponentsMove._2 == 1)) {
        opponentsMove match {
          case (0, 1) => return if (board.cellState(0, 0) == Clear) Some((0, 0)) else Some((0, 2))
          case (2, 1) => return if (board.cellState(2, 0) == Clear) Some((2, 0)) else Some((2, 2))
          case (1, 0) => return if (board.cellState(0, 0) == Clear) Some((0, 0)) else Some((2, 0))
          case (1, 2) => return if (board.cellState(2, 2) == Clear) Some((2, 2)) else Some((0, 2))
        }
      }
      return None
    }

    /*
X..
O..
...

X..
O..
X..
     */
    
    // take center if available
    if(board.cellState(1, 1) == Clear) {
      return Some(1, 1)
    } 
    
    takeOppositeCornerIfHaveCenterAndOpponentTookCorner() match {
      case None =>
      case Some(square) => return Some(square)
    }

    takeAdjacentCornerIfHaveCornerAndOpponentTookCenter() match {
      case None =>
      case Some(square) => return Some(square)
    }

    takeAdjacentCornerIfOpponentTookEdge() match {
      case None =>
      case Some(square) => return Some(square)
    }

    return None
  }
  
  def fourthMove(board: Board): Option[(Int, Int)] = {
    val myMove = board.occupiedSquares(icon).head
    val opponentsMoves = board.occupiedSquares(opponent)
    
    // takes edge if have center and opponent has opposite corners
    if(myMove == (1,1)) {
      // if both corners and corners are opposite
      if(opponentsMoves.filter(Board.corners.contains(_)).size == 2) {
        if(opponentsMoves.contains((0, 0)) && opponentsMoves.contains((2, 2))) {
          return Some(0,1)
        }
        if(opponentsMoves.contains((0, 2)) && opponentsMoves.contains((2, 0))) {
          return Some(0,1)
        }
      }
    }
    return None
  }  
}