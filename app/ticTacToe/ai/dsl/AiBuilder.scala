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
  
  val usage = """
		  <pre>
    [openingRule, ] primaryRules [, exceptionRules]
    
    Examples:
		  unbeatable
		  is random
		  is random, except blocks 90% of the time
		  opens random, otherwise is unbeatable

    openingRule ::= opens [with] openingRuleName 
    
    openingRuleName ::= "randomly" | "centerOrCorner"
    
    Examples:
		  opens randomly
		  opens with centerOrCorner

    primaryRules ::= ["is" | "otherwise is"] primaryRuleName
    
    exceptionRules ::= exceptionRule [, exceptionRule...]
    
    exceptionRule ::= ["except" | "and"] exception
    
    exception ::= simpleException | probableException | removeFromPrimaryRule

    simpleException ::= "never misses a win" | "never misses a block"
    
    probableException ::= probableRule probability
    
    probableRule ::= "misses wins" | "misses blocks" | "wins" | "blocks" | "plays win"
    
    probability ::= 0-100 [% | % of the time]
    
    removeFromPrimaryRule ::= ["misses the" | "except misses the"] ruleToRemove ["rule"]
    
    ruleToRemove ::= "cornerNearOpponent" | "priority" 
    
</pre>
  """

}