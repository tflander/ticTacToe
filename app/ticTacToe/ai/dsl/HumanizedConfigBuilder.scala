package ticTacToe.ai.dsl

import ticTacToe.ai.rule._
import ticTacToe.CellState._
import ticTacToe.ai.HumanizedAi

class HumanizedConfigBuilder(icon: CellState) {

  def smartest = {
    val level = 1.0
    Seq(
      (new Opener(icon), level),
      (new Winner(icon), level),
      (new Blocker(icon), level),
      (new CornerNearOpponent(icon), level),
      (new Priority(icon), level))
  }

}

object HumanizedConfigBuilder {
  def buildAi(icon: CellState, config: String): Seq[(AiRule, Double)] = {
    val a = new HumanizedConfigBuilder(icon)
    return a.smartest
  }
}
