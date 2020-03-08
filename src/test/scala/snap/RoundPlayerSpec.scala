package snap

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snap.Main.random
import cats.implicits._
import Rank._
import Suit._

class RoundPlayerSpec extends AnyFlatSpec with Matchers {

  behavior of "a Round Player"

  it should "increase the cards in play if there is no winner" in new scope {
    val players = Players(2)
    val ipg = InProgessGame(players, List(Card(Ace, Spades)), Nil)

    roundPlayer.playRound(ipg).unsafeRunSync() should equal(RoundResult(Nil, InProgessGame(players, Nil, List(Card(Ace, Spades)))))
  }

  it should "handle a round winner correctly" in new scope {
    val players = Players(2)
    val ipg = InProgessGame(players, List(Card(Ace, Spades)), List(Card(Ace, Spades)))

    val expectedNewPlayers =
      Players(Map(PlayerId(1) -> Player(PlayerId(1), PlayerScore(2)), PlayerId(2) -> Player(PlayerId(2), PlayerScore(0))))

    roundPlayer.playRound(ipg).unsafeRunSync() should equal(
      RoundResult(List(PlayerWinsRound(PlayerId(1), 2)), InProgessGame(expectedNewPlayers, Nil, Nil))
    )
  }

  it should "handle a game winner correctly" in new scope {
    val players = Players(Map(PlayerId(1) -> Player(PlayerId(1), PlayerScore(2)), PlayerId(2) -> Player(PlayerId(2), PlayerScore(0))))
    val ipg = InProgessGame(players, Nil, List(Card(Ace, Spades)))

    roundPlayer.playRound(ipg).unsafeRunSync() should equal(
      RoundResult(List(PlayerWinsGame(Player(PlayerId(1), PlayerScore(2)))), GameOver)
    )
  }

  private trait scope {

    val simulators = new Simulators[IO] {
      override def chooseWinner(players: Players): IO[PlayerId] = players.players.head._1.pure[IO]

      override def turningACardPause(): IO[Unit] = IO.pure(())

      override def shoutingSNAPPause(): IO[Unit] = IO.pure(())

      override def appreciatingWinPause(): IO[Unit] = IO.pure(())
    }
    val roundPlayer = RoundPlayer(simulators, Card.eqBySuitAndRank)
  }
}
