package snap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Rank._

class DeckSpec extends AnyFlatSpec with Matchers {

  behavior of "a Deck of Cards"

  val deck: Deck = Deck()

  it should "have 52 cards" in {
    deck.cards.size should equal(52)
  }

  it should "have 13 cards of each suit" in {
    deck.cards.groupBy(_.suit).values.foreach { cards =>
      cards.size should equal(13)
    }
  }

  it should "have the right number of cards for each ranks" in {
    val byRank = deck.cards.groupBy(_.rank)
    def checkFour(cards: List[Card]) = cards.size should equal(4)
    checkFour(byRank(Ace))
    checkFour(byRank(Jack))
    checkFour(byRank(Queen))
    checkFour(byRank(King))
    (2 to 10).foreach(numeral => checkFour(byRank(Numeral(numeral))))
  }

}
