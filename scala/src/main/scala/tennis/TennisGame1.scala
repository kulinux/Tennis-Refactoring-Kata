package tennis

case class Game(player1: Game.Player, player2: Game.Player) {

  def player1Score() = player1.score
  def player2Score() = player2.score

  def pointsStatus(): Game.PuntuationStatus = {
    def overForty(): Boolean = player1.score >= 4 || player2.score >= 4
    def sameScore(): Boolean = player1.score == player2.score

    import Game.PuntuationStatus._
    if (sameScore()) SameScore
    else if (overForty()) OverForty
    else Normal
  }

  def score(playerName: String): Game = {
    if (Game.Types.PlayerName(playerName) == player1.name)
      Game(player1.copy(score = player1.score + 1), player2)
    else if (Game.Types.PlayerName(playerName) == player2.name)
      Game(player1, player2.copy(score = player2.score + 1))
    else
      throw UnsupportedOperationException(
        s"Player $playerName not found in game between ${player1.name} and ${player2.name}"
      )
  }
}
object Game {
  enum PuntuationStatus {
    case SameScore, OverForty, Normal
  }
  private[Game] case class Player(name: Types.PlayerName, score: Int)

  object Types {
    opaque type PlayerName = String
    object PlayerName {
      def apply(name: String): PlayerName = name
    }
  }

  def apply(player1Name: String, player2Name: String): Game =
    Game(
      Player(Types.PlayerName(player1Name), 0),
      Player(Types.PlayerName(player2Name), 0)
    )
}

class TennisGame1(player1Name: String, player2Name: String) extends TennisGame {

  private var game = Game(player1Name, player2Name)

  def wonPoint(playerName: String): Unit = {
    game = game.score(playerName)
  }

  def calculateScore(): String = {
    import Game.PuntuationStatus._
    import Formatter._
    game.pointsStatus() match {
      case SameScore => formatSameScore(game.player1Score())
      case OverForty =>
        formatOverForty(game.player1Score() - game.player2Score())
      case Normal =>
        simpleFormat(game.player1Score()) + "-" + Formatter
          .simpleFormat(game.player2Score())
    }
  }
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
