package snap

import cats.Applicative
import cats.kernel.Eq
import cats.implicits._

sealed trait RoundOutcome
case class PlayerWinsRound(playerId: PlayerId, scored: Int) extends RoundOutcome
case class PlayerWinsGame(player: Player) extends RoundOutcome

case class RoundResult(results: List[RoundOutcome], newGameState: GameState)

trait RoundPlayer[F[_]] {
  def playRound(inProgessGame: InProgessGame): F[RoundResult]
}

object RoundPlayer {

  def apply[F[_] : Applicative](simulators: Simulators[F], snapMatcher: Eq[Card]): RoundPlayer[F] =
    new RoundPlayer[F] {
      private def chooseRoundWinner(inProgessGame: InProgessGame): F[RoundResult] =
        simulators.chooseWinner(inProgessGame.players).map { winnerId =>
          {
            val scored = inProgessGame.cardsInPlay.size + 1
            val newPlayers = inProgessGame.players.score(winnerId, scored)
            RoundResult(
              results = List(PlayerWinsRound(winnerId, scored)),
              newGameState = InProgessGame(players = newPlayers, remainingCards = inProgessGame.remainingCards.tail, cardsInPlay = Nil)
            )
          }
        }

      private def noWinner(card: Card, inProgessGame: InProgessGame): F[RoundResult] =
        simulators.turningACardPause().map { _ =>
          RoundResult(
            results = Nil,
            newGameState = inProgessGame.copy(
              remainingCards = inProgessGame.remainingCards.tail,
              cardsInPlay = card :: inProgessGame.cardsInPlay
            )
          )
        }

      private def chooseGameWinner(inProgessGame: InProgessGame): F[RoundResult] = {
        val winner = inProgessGame.players.scoreBoard.players.head
        RoundResult(
          results = List(PlayerWinsGame(winner)),
          newGameState = GameOver
        ).pure[F]
      }

      override def playRound(inProgessGame: InProgessGame): F[RoundResult] = {
        val maybeNextFaceUpCard = inProgessGame.remainingCards.headOption
        val maybeCurrentFaceUpCard = inProgessGame.cardsInPlay.headOption
        (maybeNextFaceUpCard, maybeCurrentFaceUpCard) match {
          case (Some(nextCard), Some(currentCard)) if snapMatcher.eqv(nextCard, currentCard) => chooseRoundWinner(inProgessGame)
          case (Some(nextCard), _)                                                           => noWinner(nextCard, inProgessGame)
          case (_, _)                                                                        => chooseGameWinner(inProgessGame)
        }
      }
    }
}
