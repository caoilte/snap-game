package snap

import cats.effect.Sync
import cats.implicits._

trait GameUpdatePrinter[F[_]] {
  def print(card: Card): F[Unit]
  def print(roundOutcome: RoundOutcome): F[Unit]
}

object GameUpdatePrinter {

  def apply[F[_] : Sync](): GameUpdatePrinter[F] = new GameUpdatePrinter[F] {
    private def print(toPrint: String): F[Unit] = println(toPrint).pure[F]

    def print(roundOutcome: RoundOutcome): F[Unit] = roundOutcome match {
      case PlayerWinsRound(PlayerId(id), scored) =>
        print(s"Player $id shouts SNAP and wins the round gaining $scored points!").flatTap(_ => print(""))
      case PlayerWinsGame(Player(PlayerId(id), PlayerScore(score))) =>
        print("").flatTap(_ => print(s"Player $id wins the game with $score points")).flatTap(_ => print(""))
    }

    def print(card: Card): F[Unit] = {
      val rankString = card.rank match {
        case Rank.Numeral(numeral) => numeral.toString
        case other                 => other.toString
      }
      print((s"$rankString of ${card.suit} was played"))
    }
  }
}
