package ticTacToe
import ticTacToe.CellState._
import ticTacToe.ai.dsl.TicTacToeAiParser
import ticTacToe.ai.ComputerPlayer
import ticTacToe.ai.dsl.AiBuilder._

object RoundRobin {

//  class AiBuilder(icon: CellState) extends TicTacToeAiParser(icon) {
//    def buildAi(string: String) = parseAll(ruleSet, string)
//  }
//
//  val xPlayerParser = new AiBuilder(X)
//  val oPlayerParser = new AiBuilder(O)

  def buildSchedule(players: Seq[String]): Seq[(String, String)] = {
    require(players.size > 1, "must have at least two players")

    def numRoundsForEachPlayerAsX = players.size - 1

    val a = for (playerAsX <- players) yield for (playerAsO <- players)
      yield if (playerAsX != playerAsO) Some((playerAsX, playerAsO)) else None

    return a.flatten.flatten
  }

  def runMatch(roundsPerMatch: Int, matchUp: (String, String)): Seq[(String, Int)] = {

    def play(x: ComputerPlayer, o: ComputerPlayer): CellState = {
      val game = new Game
      val board = game.play(x, o)
      return board.winner
    }

    val xName = matchUp._1
    val oName = matchUp._2
    val x = buildAi(X, xName)
    val o = buildAi(O, oName)

    val resultDetail = for (i <- 1 to roundsPerMatch) yield play(x, o)
    val resultGroups = resultDetail.groupBy(_.toString())

    def countResultsFor(icon: CellState) = {
      resultGroups.get(icon.toString) match {
        case Some(ties: Seq[CellState]) => ties.size
        case None => 0
      }
    }
    
    val numTies = countResultsFor(Clear)
    val pointsForX = (countResultsFor(X) * 2) + numTies
    val pointsForO = (countResultsFor(O) * 2) + numTies
    return Seq( (xName, pointsForX), (oName, pointsForO))
  }

}