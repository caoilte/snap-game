package snap

import cats.effect.Sync
import cats.implicits._

case class PlayerId(value: Int)
case class PlayerScore(value: Int)
case class Player(id: PlayerId, score: PlayerScore) {
  def withAdditionalScore(additionalScore: Int): Player = this.copy(score = PlayerScore(score.value + additionalScore))
}

object Player {
  def newPlayer(id: Int): Player = Player(PlayerId(id), PlayerScore(0))
}
case class ScoreBoard(players: List[Player])

case class Players(players: Map[PlayerId, Player]) {

  def score(id: PlayerId, additionalScore: Int): Players =
    Players(players.updatedWith(id) {
      case Some(player) => player.withAdditionalScore(additionalScore).some
      case None         => throw new RuntimeException(s"Unexpected $id")
    })

  def scoreBoard: ScoreBoard = ScoreBoard(players.values.toList.sortBy(_.score.value).reverse)
  def size: Int = players.size
}

object Players {

  def apply(numberOfPlayers: Int): Players =
    Players((1 to numberOfPlayers).map(id => (PlayerId(id) -> Player.newPlayer(id))).toMap)
}
