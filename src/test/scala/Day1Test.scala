import Day1.{fuelCalculation, fuelCounterUpper}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day1Test extends AnyFreeSpec with Matchers {

  "fuelCalculation" - {
    "Should return 2" - {
      "When 12 is passed" in {
        fuelCalculation(12) shouldBe 2
      }
      "When 14 is passed" in {
        fuelCalculation(14) shouldBe 2
      }
    }
    "Should return 654" - {
      "When 1969 is passed" in {
        fuelCalculation(1969) shouldBe 654
      }
    }

    "Should return 33583" - {
      "When 100756 is passed" in {
        fuelCalculation(100756) shouldBe 33583
      }
    }
  }

  "fuelCounterUpper" - {
    "Should return 34237" - {
      "When 100756, 1969 is passed" in {
        fuelCounterUpper(List(100756, 1969)) shouldBe 34237
      }
    }
  }

  "cumulativeFuel" - {
    "Should return 966" - {
      "When 1969 is passed" in {
        Day1.cumulativeFuel(1969) shouldBe 966
      }
    }

    "Should return 50346" - {
      "When 100756 is passed" in {
        Day1.cumulativeFuel(100756) shouldBe 50346
      }
    }
  }

  "cumulativeFuelCounterUpper" - {
    "Should return 51312" - {
      "When 1969, 100756 is passed" in {
        Day1.cumulativeFuelCounterUpper(List(1969, 100756)) shouldBe 51312
      }
    }
  }

}
