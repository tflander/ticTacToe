package ticTacToe.ai.rule
import ticTacToe.Board

case class ProbableRule(val baseRule: AiRule, val probability: Double) extends AiRule {

  override def squareToPlay(board: Board): Option[(Int, Int)] = {

    val rnd = random.nextDouble

    if (rnd <= probability) {
      return baseRule.squareToPlay(board)
    }
    return None
  }
}
