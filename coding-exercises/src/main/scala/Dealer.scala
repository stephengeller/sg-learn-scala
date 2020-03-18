class Dealer {
  def foo(line: String): Int = ???

  def hasBlackjack(cards: List[Card]): Boolean = {
    determineScore(cards) == 21
  }

  def determineScore(cards: List[Any]): Int = {
    cards.map {
      case card: Int    => card;
      case card: String => Dealer.getFaceCardValue(card)
    }.sum
  }

}

object Dealer {
  def getFaceCardValue(card: String): Int = {
    card match {
      case "J" => 10;
      case "Q" => 10;
      case "K" => 10;
      case "A" => 11
    }
  }
}

case class Card(cardType: Any)
