package snap

import cats.Monad
import cats.implicits._

sealed trait GameState
case object GameOver extends GameState
case class InProgessGame(players: Players, remainingCards: List[Card], cardsInPlay: List[Card]) extends GameState

object InProgessGame {
  def apply(players: Players, remainingCards: List[Card]): InProgessGame = InProgessGame(players, remainingCards, Nil)
}

trait GamePlayer[F[_]] {
  def playGame(inProgessGame: InProgessGame): F[Unit]
}

class DefaultGamePlayer[F[_] : Monad](gameUpdatePrinter: GameUpdatePrinter[F], roundPlayer: RoundPlayer[F]) extends GamePlayer[F] {

  final def gameLoop(inProgessGame: InProgessGame): F[GameState] =
    for {
      _ <- inProgessGame.remainingCards.headOption.fold(().pure[F])((card: Card) => gameUpdatePrinter.print(card))
      result <- roundPlayer.playRound(inProgessGame)
      _ <- result.results.traverse(outcome => gameUpdatePrinter.print(outcome))
    } yield result.newGameState

  override def playGame(inProgessGame: InProgessGame): F[Unit] =
    Monad[F].tailRecM[GameState, Unit](inProgessGame) {
      case GameOver           => ().pure[F].map(Right(_))
      case ipg: InProgessGame => gameLoop(ipg).map(Left(_))
    }
}
