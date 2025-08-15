package tennis

case class Player(name: String, score: Int)

case class Game(player1: Player, player2: Player) {
  def overForty(): Boolean = player1.score >= 4 || player2.score >= 4

  def sameScore(): Boolean = player1.score == player2.score

  def score(playerName: String): Game = {
    if (playerName == player1.name)
      Game(player1.copy(score = player1.score + 1), player2)
    else if (playerName == player2.name)
      Game(player1, player2.copy(score = player2.score + 1))
    else
      throw UnsupportedOperationException(
        s"Player $playerName not found in game between ${player1.name} and ${player2.name}"
      )
  }
}
object Game {
  def apply(player1Name: String, player2Name: String): Game =
    Game(Player(player1Name, 0), Player(player2Name, 0))
}

object Formatter {
  def formatSameScore(score: Int): String =
    score match {
      case 0 => "Love-All"
      case 1 => "Fifteen-All"
      case 2 => "Thirty-All"
      case _ => "Deuce"
    }

  def formatOverForty(minusResult: Int): String =
    if (minusResult == 1) "Advantage player1"
    else if (minusResult == -1) "Advantage player2"
    else if (minusResult >= 2) "Win for player1"
    else "Win for player2"

  def simpleFormat(score: Int): String =
    score match {
      case 0 => "Love"
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
    }
}

class TennisGame1(player1Name: String, player2Name: String) extends TennisGame {

  var game = Game(player1Name, player2Name)

  def wonPoint(playerName: String): Unit = {
    game = game.score(playerName)
  }

  def calculateScore(): String = {
    if (game.sameScore()) {
      Formatter.formatSameScore(game.player1.score)
    } else if (game.overForty()) {
      Formatter.formatOverForty(game.player1.score - game.player2.score)
    } else {
      Formatter.simpleFormat(game.player1.score) +
        "-" +
        Formatter.simpleFormat(game.player2.score)
    }
  }

}
