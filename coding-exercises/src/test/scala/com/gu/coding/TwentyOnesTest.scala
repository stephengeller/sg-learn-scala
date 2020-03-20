package com.gu.coding

import com.gu.coding.TwentyOnes._
import org.scalatest.{FreeSpec, Matchers}

class TwentyOnesTest extends FreeSpec with Matchers {
  "score" - {
    "scores 0 for an empty hand" in {
      score(Hand(Nil)) shouldBe 0
    }
    "scores 21 for a jack and an ace" in {
      score(Hand(List(Jack, Ace))) shouldBe 21
    }
    "scores 5 for a two and a three" in {
      score(Hand(List(Two, Three))) shouldBe 5
    }
    "scores 10 for a two, a three and a five" in {
      score(Hand(List(Two, Three, Five))) shouldBe 10
    }
    "scores 30 for a jack, a ten and a queen" in {
      score(Hand(List(Jack, Ten, Queen))) shouldBe 30
    }
  }

  "hasBlackjack" - {
    "returns true if blackjack with jack and ace" in {
      hasBlackjack(Hand(List(Jack, Ace))) shouldBe true
    }

    "returns true if 21 with 10 and ace" in {
      hasBlackjack(Hand(List(Ten, Ace))) shouldBe true
    }
    "returns false if 21 with jack, five and six" in {
      hasBlackjack(Hand(List(Jack, Five, Six))) shouldBe false
    }
    "returns false if not blackjack with five and six" in {
      hasBlackjack(Hand(List(Five, Six))) shouldBe false
    }
  }

  "drawCard" - {
    "removes top card of deck and places it into hand" in {
      val (hand, deck) = drawCard(Hand(Nil), Deck(List(Jack, Queen, Two)))
      hand shouldBe Hand(List(Jack))
      deck shouldBe Deck(List(Queen, Two))
    }
    "cannot draw card if deck is empty" in {
      assertThrows[NoSuchElementException] {
        drawCard(Hand(Nil), Deck(Nil))
      }
    }
  }
}
