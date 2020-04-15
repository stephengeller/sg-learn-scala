package com.gu.coding

import com.gu.coding.TwentyOnes._
import org.scalatest.{FreeSpec, Matchers, OptionValues}

class TwentyOnesTest extends FreeSpec with Matchers with OptionValues {
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

  "drawSam" - {
    "removes top card of deck" in {
      val before =
        Game(Hand(Nil), Hand(Nil), Deck(List(Jack, Queen, Two)), None)
      drawSam(before).deck shouldBe Deck(List(Queen, Two))
    }
    "puts top card into Sam's hand" in {
      val before =
        Game(Hand(Nil), Hand(Nil), Deck(List(Jack, Queen, Two)), None)
      drawSam(before).sam shouldBe Hand(List(Jack))
    }

    "puts top 2 cards into Sam's hand" in {
      val before = Game(Hand(Nil), Hand(Nil), Deck(List(Queen, Four)), None)
      drawSam(drawSam(before)).sam shouldBe Hand(List(Queen, Four))
    }

    "cannot draw card if deck is empty" in {
      val before = Game(Hand(Nil), Hand(Nil), Deck(Nil), None)
      assertThrows[NoSuchElementException] {
        drawSam(before)
      }
    }
  }

  "drawDealer" - {
    val startingDeck = Deck(List(Seven, Five, Three))
    "removes top card of deck" in {
      val before = Game(Hand(Nil), Hand(Nil), startingDeck, None)
      drawDealer(before).deck shouldBe Deck(List(Five, Three))
    }
    "puts top card into dealer's hand" in {
      val before = Game(Hand(Nil), Hand(Nil), startingDeck, None)
      drawDealer(before).dealer shouldBe Hand(List(Seven))
    }

    "puts top 2 cards into dealer's hand" in {
      val before = Game(Hand(Nil), Hand(Nil), startingDeck, None)
      drawDealer(drawDealer(before)).dealer shouldBe Hand(List(Seven, Five))
    }

    "cannot draw card if deck is empty" in {
      val before = Game(Hand(Nil), Hand(Nil), Deck(Nil), None)
      assertThrows[NoSuchElementException] {
        drawDealer(before)
      }
    }
  }

  "playTurn" - {
    val startingDeck = Deck(List(Seven, Five, Three))

    "returns Sam as winner if they have 21" in {
      val game = Game(
        sam = Hand(List(Ten, Ace)),
        dealer = Hand(Nil),
        deck = startingDeck,
        None
      )
      playTurn(game).winner.value shouldEqual "Sam"
    }

    "returns Dealer as winner if Dealer gets 21" in {
      val game = Game(
        sam = Hand(Nil),
        dealer = Hand(List(Ten, Ace)),
        deck = startingDeck,
        None
      )
      playTurn(game).winner.value shouldEqual "Dealer"
    }

    "draws Sam another card if they have less than 17" in {
      val game = Game(
        sam = Hand(List(One, Three)),
        dealer = Hand(Nil),
        deck = startingDeck,
        None
      )
      playTurn(game).sam.cards.length shouldEqual 3
    }

    "returns Dealer as winner if Sam has more than 21" in {
      val game = Game(
        sam = Hand(List(Ten, Three, Ten)),
        dealer = Hand(Nil),
        deck = startingDeck,
        None
      )
      playTurn(game).winner.value shouldEqual "Dealer"
    }

    "doesn't draw Sam a card if Sam has more than 17" in {
      val game = Game(
        sam = Hand(List(Ten, Ten)),
        dealer = Hand(Nil),
        deck = startingDeck,
        None
      )
      playTurn(game).sam.cards.length shouldEqual 2
    }

    "draws Dealer another card if they have less than Sam" in {
      val game = Game(
        sam = Hand(List(Ten, Eight)),
        dealer = Hand(List(Three, Four)),
        deck = startingDeck,
        None
      )
      playTurn(game).dealer.cards.length shouldEqual 3
    }

    "returns Sam as winner if Dealer has more than 21" in {
      val game = Game(
        sam = Hand(List(Ten, Eight)),
        dealer = Hand(List(Three, Four, Ten, Ten)),
        deck = startingDeck,
        None
      )
      playTurn(game).winner.value shouldEqual "Sam"
    }

    "returns Dealer as winner if Dealer has more than Sam" in {
      val game = Game(
        sam = Hand(List(Ten, Eight)),
        dealer = Hand(List(Ten, Eight, Two)),
        deck = startingDeck,
        None
      )
      playTurn(game).winner.value shouldEqual "Dealer"
    }
  }
}
