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
}
