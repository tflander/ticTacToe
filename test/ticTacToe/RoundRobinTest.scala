package ticTacToe
import org.scalatest._

class RoundRobinTest extends FunSpec with ShouldMatchers {

  import RoundRobin._
  
  describe("when buildSchedule") {
    it("should fail an empty list") {
      intercept[IllegalArgumentException] {
        buildSchedule(Nil)
      }
    }

    it("should fail if only one player") {
      intercept[IllegalArgumentException] {
        buildSchedule(List("player1"))
      }
    }

    it("should create two matches for two players") {
      val matches = buildSchedule(Seq("player1", "player2"))
      matches should be(Seq(
        ("player1", "player2"),
        ("player2", "player1")))
    }

    it("should create matches for four players") {
      val matches = buildSchedule(Seq("player1", "player2", "player3", "player4"))
      matches.toSet should be(Set(
        ("player1", "player2"),
        ("player3", "player4"),

        ("player1", "player3"),
        ("player2", "player4"),

        ("player1", "player4"),
        ("player2", "player3"),

        ("player2", "player1"),
        ("player4", "player3"),

        ("player3", "player1"),
        ("player4", "player2"),

        ("player4", "player1"),
        ("player3", "player2")))
    }
  }

}