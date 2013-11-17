package ticTacToe

import ticTacToe.ai.ComputerPlayer
import ticTacToe.CellState._
import ticTacToe.ai.dsl.AiBuilder._

object AiVersesAiMatchRunner {
  def runMatch(roundsPerMatch: Int, matchUp: (String, String)): Seq[Score] = {

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
    val xWins = countResultsFor(X)
    val oWins = countResultsFor(O)
    val pointsForX = (xWins * 2) + numTies
    val pointsForO = (oWins * 2) + numTies
    return Seq( 
        Score(xName, xWins, oWins, numTies, pointsForX),
        Score(oName, oWins, xWins, numTies, pointsForO))
  }

}

case class Score(player: String, wins: Int, losses: Int, ties: Int, points: Int)