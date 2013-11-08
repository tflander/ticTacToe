package ticTacToe.ai
import org.scalatest._
import ticTacToe.ai.dsl._
import ticTacToe.CellState._
import scala.util.parsing.combinator.JavaTokenParsers
import ticTacToe.ai.rule.AiRule
import ticTacToe.CellState._

class ConfigSpike(icon: CellState) extends TicTacToeAiParser(icon) {
  def buildAi(string: String) = parseAll(ruleSet, string)
}

class HumanizedConfigTest extends FunSpec with ShouldMatchers {

  val configBuilder = new ConfigSpike(X)

  describe("Humanized Config Tests") {

    it("should create an unbeatable AI") {
      val aiRules = configBuilder.buildAi("is unbeatable");
      aiRules.successful should be(true)
      println(aiRules.get)
      /*
(
  (
    None 
    ~ 
    List(
      ticTacToe.ai.rule.Opener@79ee2c2c, 
      ticTacToe.ai.rule.Winner@3963b3e, 
      ticTacToe.ai.rule.Blocker@7c0b6548, 
      ticTacToe.ai.rule.CornerNearOpponent@50269997, 
      ticTacToe.ai.rule.Priority@162db19d))
      ~
      None
  )

       */
      // TODO:  validation
    }

    it("should create an AI that opens randomly") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("should create an AI that opens with center or corner, then moves randomly") {
      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("should create an AI that opens with center or corner, then moves randomly, misses blocks 10% of the time") {
      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random, blocks 90% of the time, never misses a win");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("same as above, but can use 'and' to chain exception rules") {
      val aiRules = configBuilder.buildAi("opens with center or corner, otherwise is random, blocks 90% of the time, and never misses a win");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("supports using whitespace to stack vertically") {
      val aiRules = configBuilder.buildAi("""
          opens with center or corner, 
    		  otherwise is random, 
    		  plays win 90% of the time, 
    		  and never misses a block
          """);
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("should create an AI that opens randomly, plays strong, but sometimes misses a block") {
      val aiRules = configBuilder.buildAi("opens randomly, otherwise is unbeatable, except misses blocks 10% of the time");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("should create an AI that is generally unbeatable, but sometimes misses a win") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses wins 10% of the time");
      aiRules.successful should be(true)
      // TODO:  validation
    }

    it("can short-hand an exception") {
      val aiRules = configBuilder.buildAi("is unbeatable, except misses wins 10%");
      println(aiRules)
      aiRules.successful should be(true)
      // TODO:  validation      
    }

  }
}
