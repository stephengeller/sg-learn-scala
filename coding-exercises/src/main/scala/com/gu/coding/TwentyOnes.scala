package com.gu.coding

object TwentyOnes {
  def main(args: Array[String]): Unit = {
    println(args(0))
  }

  def score(hand: Hand): Int = hand.cards.map(card => rankValue(card)).sum

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

}

case class Hand(cards: List[Rank])

case class Card(rank: Rank)

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
