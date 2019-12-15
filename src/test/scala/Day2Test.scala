import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFreeSpec with Matchers {

  "computeIntcode" - {
    "Should Add" - {
      "When OpCode is 1" in {
        val input = List(1, 0, 0, 0, 99)
        Day2.computeIntcode(input) shouldBe List(2, 0, 0, 0, 99)
      }
    }
    "Should Multiply" - {
      "When OpCode is 2" in {
        val input = List(2, 3, 0, 3, 99)
        Day2.computeIntcode(input) shouldBe List(2, 3, 0, 6, 99)
      }
    }
    "Should be able to set value portioned after halt OpCode" in {
      val input = List(2, 4, 4, 5, 99, 0)
      Day2.computeIntcode(input) shouldBe List(2, 4, 4, 5, 99, 9801)
    }
    "Should be able to process multiple OpCodes" in {
      val input = List(1, 1, 1, 4, 99, 5, 6, 0, 99)
      Day2.computeIntcode(input) shouldBe List(30, 1, 1, 4, 2, 5, 6, 0, 99)
    }
  }
}
