package snap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snap.Rank._

class PlayersSpec extends AnyFlatSpec with Matchers {

  behavior of "A 2 player game"

  val players: Players = Players(2)

  it should "have 2 players" in {
    players.size should equal(2)
  }

  it should "have a zero score for every player to begin with" in {
    players.players.values.map(_.score.value).foreach(_ should equal(0))
  }

  it should "score a player correctly" in {
    players.score(PlayerId(1), 2).scoreBoard should equal(
      ScoreBoard(List(Player(PlayerId(1), PlayerScore(2)), Player(PlayerId(2), PlayerScore(0))))
    )
    players.score(PlayerId(1), 2).score(PlayerId(2), 3).score(PlayerId(1), 2).scoreBoard should equal(
      ScoreBoard(List(Player(PlayerId(1), PlayerScore(4)), Player(PlayerId(2), PlayerScore(3))))
    )
  }

}
