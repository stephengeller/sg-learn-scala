package com.gu.coding

object TwentyOnes {
  def main(args: Array[String]): Unit = {
    // Start
    val game = initialState()
    val winner = run(game)
    println(s"The winner is $winner")
  }

  def initialState(): Game = {
    val deck = Deck(List(One, Jack, Two, Four, Ace))
    val (sam1, deck1) = drawCard(Hand(Nil), deck)
    val (dealer1, deck2) = drawCard(Hand(Nil), deck1)
    val (sam2, deck3) = drawCard(sam1, deck2)
    val (dealer2, deck4) = drawCard(dealer1, deck3)
    Game(sam2, dealer2, deck4)
  }

  def score(hand: Hand): Int = hand.cards.map(rankValue).sum

  def rankValue(rank: Rank): Int = {
    rank match {
      case One   => 1
      case Two   => 2
      case Three => 3
      case Four  => 4
      case Five  => 5
      case Six   => 6
      case Seven => 7
      case Eight => 8
      case Nine  => 9
      case Ten   => 10
      case Jack  => 10
      case Queen => 10
      case King  => 10
      case Ace   => 11
    }
  }

  def hasBlackjack(initialHand: Hand): Boolean =
    score(initialHand) == 21 && initialHand.cards.length == 2

  def drawCard(hand: Hand, deck: Deck): (Hand, Deck) = {
    val cardDrawn = deck.cards.head
    val newHand: Hand = Hand(cardDrawn :: hand.cards)
    val newDeck: Deck = Deck(deck.cards.slice(1, deck.cards.length))
    (newHand, newDeck)
  }

  def run(game: Game): String = ???
}

case class Game(sam: Hand, dealer: Hand, deck: Deck)

case class Deck(cards: List[Rank])

case class Hand(cards: List[Rank])

//case class Card(rank: Rank)

sealed trait Rank
object One extends Rank
object Two extends Rank
object Three extends Rank
object Four extends Rank
object Five extends Rank
object Six extends Rank
object Seven extends Rank
object Eight extends Rank
object Nine extends Rank
object Ten extends Rank
object Jack extends Rank
object Queen extends Rank
object King extends Rank
object Ace extends Rank
