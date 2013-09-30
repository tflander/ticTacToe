package ticTacToe.ai.rule
import ticTacToe.Board
import scala.util.Random
import java.util.Date

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
  
  def takeCenter(): Option[(Int, Int)] = Some(1,1)
}