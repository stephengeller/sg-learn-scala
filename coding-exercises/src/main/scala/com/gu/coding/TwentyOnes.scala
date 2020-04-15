package com.gu.coding

import scala.annotation.tailrec

object TwentyOnes {
  val initialDeck = List(One, Jack, Two, Four, Ace)

  def main(args: Array[String]): Unit = {
    // Start
    val game = initialState()
    val winner = run(game)
    println(s"The winner is $winner")
  }

  def initialState(): Game = {
    val start = Game(Hand(Nil), Hand(Nil), Deck(initialDeck), None)
    drawSam(drawDealer(drawSam(drawDealer(start))))
  }

  def drawSam(game: Game): Game = {
    val cardDrawn = game.deck.cards.head
    game.copy(
      sam = Hand(game.sam.cards :+ cardDrawn),
      deck = Deck(game.deck.cards.tail)
    )
  }

  def drawDealer(game: Game): Game = {
    val cardDrawn = game.deck.cards.head
    game.copy(
      dealer = Hand(game.dealer.cards :+ cardDrawn),
      deck = Deck(game.deck.cards.tail)
    )
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

  def playTurn(game: Game): Game = {
    if (hasBlackjack(game.sam))
      game.copy(winner = Some("Sam"))
    else if (hasBlackjack(game.dealer))
      game.copy(winner = Some("Dealer"))
    else if (score(game.sam) < 17)
      drawSam(game)
    else if (score(game.sam) > 21)
      game.copy(winner = Some("Dealer"))
    else if (score(game.dealer) < score(game.sam))
      drawDealer(game)
    else if (score(game.dealer) > 21)
      game.copy(winner = Some("Sam"))
    else
      game.copy(winner = Some("Dealer"))
  }

  @tailrec
  def run(game: Game): String = {
    val result = playTurn(game)

    result.winner match {
      case Some(winner) => winner
      case None         => run(result)
    }
  }
}

case class Game(sam: Hand, dealer: Hand, deck: Deck, winner: Option[String])

case class Deck(cards: List[Rank])

case class Hand(cards: List[Rank])

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
