package ticTacToe

import ticTacToe.ai.ComputerPlayer
import ticTacToe.CellState._
import ticTacToe.ai.dsl.AiBuilder._

object AiVersesAiMatchRunner {
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