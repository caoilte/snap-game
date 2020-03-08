package snap

import cats.Id
import cats.effect.{IO, Sync}
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snap.Main.random
import snap.Rank._
import snap.Suit._

class GamePlayerSpec extends AnyFlatSpec with Matchers {

  behavior of "a Game Player"

  it should "stop if game ends after one loop" in new scope {
    val players = Players(2)
    val ipg = InProgessGame(players, List(Card(Ace, Spades)), Nil)

    gamePlayerWithRoundResults(List(RoundResult(Nil, GameOver))).playGame(ipg)
    printedCards should equal(List(Card(Ace, Spades)))
    printedRoundOutcomes should equal(Nil)
    inProgressGames should equal(List(ipg))
  }

  it should "record a one result game loop" in new scope {
    val players = Players(2)
    val ipg = InProgessGame(players, List(Card(Ace, Spades)), Nil)

    val roundResults = List(
      RoundResult(List(PlayerWinsRound(PlayerId(1), 2)), ipg),
      RoundResult(Nil, GameOver)
    )

    gamePlayerWithRoundResults(roundResults).playGame(ipg)
    printedCards should equal(List(Card(Ace, Spades), Card(Ace, Spades)))
    printedRoundOutcomes should equal(List(PlayerWinsRound(PlayerId(1), 2)))
    inProgressGames should equal(List(ipg, ipg))
  }

  private trait scope {
    var printedCards: List[Card] = Nil
    var printedRoundOutcomes: List[RoundOutcome] = Nil
    var inProgressGames: List[InProgessGame] = Nil

    val printer = new GameUpdatePrinter[Id] {
      override def print(card: Card): Id[Unit] =
        printedCards = card :: printedCards

      override def print(roundOutcome: RoundOutcome): Id[Unit] =
        printedRoundOutcomes = roundOutcome :: printedRoundOutcomes
    }

    def gamePlayerWithRoundResults(roundResults: List[RoundResult]): DefaultGamePlayer[Id] = {
      var roundResultsLeft = roundResults

      val roundPlayer = new RoundPlayer[Id] {
        override def playRound(inProgessGame: InProgessGame): Id[RoundResult] = {
          inProgressGames = inProgessGame :: inProgressGames
          roundResultsLeft match {
            case head :: tail => {
              roundResultsLeft = tail
              head
            }
            case Nil => throw new Throwable("Ran out of test rounds to return")
          }
        }
      }
      new DefaultGamePlayer[Id](printer, roundPlayer)

    }
  }
}
