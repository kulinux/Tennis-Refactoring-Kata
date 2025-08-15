package tennis

case class Player(name: String, score: Int)

case class Game(player1: Player, player2: Player) {
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

class TennisGame1(player1Name: String, player2Name: String) extends TennisGame {

  var game = Game(player1Name, player2Name)

  def wonPoint(playerName: String): Unit = {
    game = game.score(playerName)
  }

  def calculateScore(): String = {
    var score: String = ""
    var tempScore = 0
    if (game.player1.score == game.player2.score) {
      score = game.player1.score match {
        case 0 => "Love-All"
        case 1 => "Fifteen-All"
        case 2 => "Thirty-All"
        case _ => "Deuce"

      }
    } else if (game.player1.score >= 4 || game.player2.score >= 4) {
      val minusResult = game.player1.score - game.player2.score
      if (minusResult == 1) score = "Advantage player1"
      else if (minusResult == -1) score = "Advantage player2"
      else if (minusResult >= 2) score = "Win for player1"
      else score = "Win for player2"
    } else {
      for (i <- 1 until 3 by 1) {
        if (i == 1) tempScore = game.player1.score
        else { score += "-"; tempScore = game.player2.score; }
        val tempScore2 = tempScore match {
          case 0 => "Love"
          case 1 => "Fifteen"
          case 2 => "Thirty"
          case 3 => "Forty"
        }
        score += tempScore2
      }
    }
    return score
  }

}
