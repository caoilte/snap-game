package snap

import cats.effect.{ExitCode, IO, IOApp}
import java.io.BufferedReader
import java.io.InputStreamReader

import cats.implicits._
import cats.kernel.Eq

import scala.annotation.tailrec
import scala.util.Random

object Main extends IOApp {
  val reader = new BufferedReader(new InputStreamReader(System.in))
  val random = new Random(System.currentTimeMillis)

  @tailrec
  def getNumberOfDecks(): Int = {
    print("How many playing card decks do you want to play with? ")
    reader.readLine.trim.toIntOption.flatMap(i => if (i >= 1) Option(i) else None) match {
      case Some(decks) => decks
      case None => {
        println("Must be a number greater than zero")
        getNumberOfDecks()
      }
    }
  }

  @tailrec
  def chooseMatcher(): Eq[Card] = {
    print("Do you want to match on 'suit', 'value', or 'both'? ")
    reader.readLine.trim.toLowerCase match {
      case "suit"  => Card.eqBySuit
      case "value" => Card.eqByRank
      case "both"  => Card.eqBySuitAndRank
      case _ => {
        println("Must be 'suit', 'value', or 'both'")
        chooseMatcher()
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    println("Welcome to a game of Snap!")
    println("")
    val cards = Deck.numberOfShuffledDecks(random, getNumberOfDecks())
    val matcher = chooseMatcher()

    val gamePlayer = new DefaultGamePlayer[IO](GameUpdatePrinter[IO](), RoundPlayer(Simulators[IO](random), matcher))
    println("")

    gamePlayer.playGame(InProgessGame(Players(2), cards, Nil)).as(ExitCode.Success)
  }
}
