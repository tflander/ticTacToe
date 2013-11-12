package ticTacToe.ai.dsl
import ticTacToe.CellState._
import ticTacToe.ai.ComputerPlayer
import ticTacToe.ai.HumanizedAi

object AiBuilder {
  
  class AiParser(icon: CellState) extends TicTacToeAiParser(icon) {
    def buildAi(string: String) = parseAll(ruleSet, string)
  }
  
  def xAiParser = new AiParser(X)
  def oAiParser = new AiParser(O)
  
  def buildAi(icon: CellState, rules: String): HumanizedAi = {
    val builder = icon match {
      case X => xAiParser
      case O => oAiParser
    }
    val a = builder.buildAi(rules)
    require(a.successful, a.toString)
    return a.get
  }

}