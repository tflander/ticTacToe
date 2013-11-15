package ticTacToe
import ticTacToe.CellState._
import ticTacToe.ai.dsl.TicTacToeAiParser
import ticTacToe.ai.ComputerPlayer
import ticTacToe.ai.dsl.AiBuilder._

object RoundRobin {

  def buildSchedule(players: Seq[String]): Seq[(String, String)] = {
    require(players.size > 1, "must have at least two players")

    def numRoundsForEachPlayerAsX = players.size - 1

    val a = for (playerAsX <- players) yield for (playerAsO <- players)
      yield if (playerAsX != playerAsO) Some((playerAsX, playerAsO)) else None

    return a.flatten.flatten
  }

}