package snap

import cats.kernel.Eq

import scala.util.Random

sealed trait Suit

object Suit {
  case object Spades extends Suit
  case object Clubs extends Suit
  case object Hearts extends Suit
  case object Diamonds extends Suit
}
sealed trait Rank

object Rank {
  case object Ace extends Rank
  case class Numeral(number: Int) extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
}

case class Card(rank: Rank, suit: Suit)

object Card {
  val eqBySuit: Eq[Card] = (x: Card, y: Card) => x.suit == y.suit
  val eqByRank: Eq[Card] = (x: Card, y: Card) => x.rank == y.rank
  val eqBySuitAndRank: Eq[Card] = Eq.fromUniversalEquals
}

case class Deck(cards: List[Card])

object Deck {
  import Rank._
  import Suit._

  private def suitCards(suit: Suit): List[Card] =
    (Ace :: (2 to 10).map(Numeral).toList ::: Jack :: Queen :: King :: Nil).map(Card(_, suit))

  def apply(): Deck =
    Deck(List(Spades, Clubs, Hearts, Diamonds).flatMap(suitCards))

  def numberOfShuffledDecks(random: Random, numberOfDecks: Int): List[Card] =
    random.shuffle(List.fill(numberOfDecks)(Deck()).flatMap(_.cards))
}
