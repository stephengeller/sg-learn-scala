import org.scalatest.{FunSpec, Matchers, BeforeAndAfter}
import Matchers._

class DealerTest extends FunSpec with BeforeAndAfter {
  var dealer: Dealer = _

  before {
    dealer = new Dealer
  }

  describe("determineScore") {
    it("should add two cards to determine score") {
      dealer.determineScore(List(2, 5)) shouldEqual 7
    }

    it("should add as many cards as passed") {
      dealer.determineScore(List(2, 5, 10, 5)) shouldEqual 22
    }

    it("should be able to handle jacks, queens, kings and aces") {
      dealer.determineScore(List("J", "Q", "K", "A", 2)) shouldEqual 43
    }

    it("should be able to handle one card") {
      dealer.determineScore(List("J")) shouldEqual 10
    }
  }

//  describe("hasBlackjack") {
//    it("should return true if blackjack") {
//      val cards: List[Card] = List(Card("A"), Card("J"))
//      dealer.hasBlackjack(cards) shouldEqual true
//    }
//
//    it("should return false if not") {
//      val cards: List[Card] = List(Card(2), Card("J"))
//      dealer.hasBlackjack(cards) shouldEqual false
//    }
//
//    it("should add as many cards as passed") {
//      dealer.determineScore(List(2, 5, 10, 5)) shouldEqual 22
//    }
//
//    it("should be able to handle jacks, queens, kings and aces") {
//      dealer.determineScore(List("J", "Q", "K", "A", 2)) shouldEqual 43
//    }
//
//    it("should be able to handle one card") {
//      dealer.determineScore(List("J")) shouldEqual 10
//    }
//  }
}
