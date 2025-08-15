package tennis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers

class OneCaseTennisTest(params: TennisTestCase) extends Matchers {

  def checkScores(game: TennisGame): Unit = {
    val highestScore = Math.max(params.player1Score, params.player2Score)
    for (i <- 0 until highestScore by 1) {
      if (i < params.player1Score)
        game.wonPoint("player1")
      if (i < params.player2Score)
        game.wonPoint("player2")
    }
    params.expectedScore shouldBe game.calculateScore()
  }

  def checkGame1(): Unit = {
    val game = new TennisGame1("player1", "player2")
    checkScores(game)
  }

  def checkGame2(): Unit = {
    val game = new TennisGame2("player1", "player2")
    checkScores(game)
  }

  def checkGame3(): Unit = {
    val game = new TennisGame3("player1", "player2")
    checkScores(game)
  }

}

class TennisTest extends AnyFlatSpec with TableDrivenPropertyChecks {
  def getAllScores() =
    Table(
      ("test case"),
      new TennisTestCase(0, 0, "Love-All"),
      new TennisTestCase(1, 1, "Fifteen-All"),
      new TennisTestCase(2, 2, "Thirty-All"),
      new TennisTestCase(3, 3, "Deuce"),
      new TennisTestCase(4, 4, "Deuce"),
      new TennisTestCase(1, 0, "Fifteen-Love"),
      new TennisTestCase(0, 1, "Love-Fifteen"),
      new TennisTestCase(2, 0, "Thirty-Love"),
      new TennisTestCase(0, 2, "Love-Thirty"),
      new TennisTestCase(3, 0, "Forty-Love"),
      new TennisTestCase(0, 3, "Love-Forty"),
      new TennisTestCase(4, 0, "Win for player1"),
      new TennisTestCase(0, 4, "Win for player2"),
      new TennisTestCase(2, 1, "Thirty-Fifteen"),
      new TennisTestCase(1, 2, "Fifteen-Thirty"),
      new TennisTestCase(3, 1, "Forty-Fifteen"),
      new TennisTestCase(1, 3, "Fifteen-Forty"),
      new TennisTestCase(4, 1, "Win for player1"),
      new TennisTestCase(1, 4, "Win for player2"),
      new TennisTestCase(3, 2, "Forty-Thirty"),
      new TennisTestCase(2, 3, "Thirty-Forty"),
      new TennisTestCase(4, 2, "Win for player1"),
      new TennisTestCase(2, 4, "Win for player2"),
      new TennisTestCase(4, 3, "Advantage player1"),
      new TennisTestCase(3, 4, "Advantage player2"),
      new TennisTestCase(5, 4, "Advantage player1"),
      new TennisTestCase(4, 5, "Advantage player2"),
      new TennisTestCase(15, 14, "Advantage player1"),
      new TennisTestCase(14, 15, "Advantage player2"),
      new TennisTestCase(6, 4, "Win for player1"),
      new TennisTestCase(4, 6, "Win for player2"),
      new TennisTestCase(16, 14, "Win for player1"),
      new TennisTestCase(14, 16, "Win for player2")
    )

  it should "check game 1" in {
    forAll(getAllScores()) { params =>
      val test = new OneCaseTennisTest(params)
      test.checkGame1()
    }
  }
  it should "check game 2" in {
    forAll(getAllScores()) { params =>
      val test = new OneCaseTennisTest(params)
      test.checkGame2()
    }
  }
  it should "check game 3" in {
    forAll(getAllScores()) { params =>
      val test = new OneCaseTennisTest(params)
      test.checkGame3()
    }
  }
}
