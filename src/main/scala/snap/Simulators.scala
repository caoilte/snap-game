package snap

import cats.effect.{Sync, Timer}
import scala.concurrent.duration._
import cats.implicits._

import scala.util.Random

trait Simulators[F[_]] {
  def chooseWinner(players: Players): F[PlayerId]
  def turningACardPause(): F[Unit]
  def shoutingSNAPPause(): F[Unit]
  def appreciatingWinPause(): F[Unit]
}

object Simulators {

  def apply[F[_] : Sync : Timer](random: Random): Simulators[F] = new Simulators[F] {
    private def pause(length: FiniteDuration): F[Unit] = Timer[F].sleep(length)

    override def turningACardPause(): F[Unit] = pause((random.nextInt(500) + 750).milliseconds)
    override def shoutingSNAPPause(): F[Unit] = pause((random.nextInt(400) + 300).milliseconds)
    override def appreciatingWinPause(): F[Unit] = pause((random.nextInt(2000) + 1000).milliseconds)

    override def chooseWinner(players: Players): F[PlayerId] =
      shoutingSNAPPause().flatMap { _ =>
        val playerId = PlayerId(random.nextInt(players.size) + 1)
        players.players.get(playerId).fold(new Throwable(s"Invalid $playerId").raiseError[F, PlayerId])(p => p.id.pure[F])
      }
  }
}
