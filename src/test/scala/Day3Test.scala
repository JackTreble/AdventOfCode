import Day3.{WireDirections, WireInstruction, parseInput}
import cats.data.NonEmptyList
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day3Test extends AnyFreeSpec with Matchers {

  "parseInput" - {
    "Should return WireInstruction" - {
      "When valid string is passed" in {
        Day3.parseInput("U25") should contain(
          WireInstruction(WireDirections.Up, 25)
        )
      }
    }
    "Should return None" - {
      "When invalid string is passed" in {
        Day3.parseInput("J0") shouldBe None
      }
    }
  }

  "generateWireSegment" - {
    "Should generate segmentList for WireInstruction" in {
      Day3.generateWireSegment(
        previousSegment = ((0, 0), NonEmptyList.one(0)),
        WireInstruction(WireDirections.Up, 3)
      ) shouldEqual List(
        (0, 1) -> NonEmptyList.one(1),
        (0, 2) -> NonEmptyList.one(2),
        (0, 3) -> NonEmptyList.one(3),
      )
    }
  }

  "generateWireMap" - {
    "Should generate a wireMap for WireInstruction" in {
      Day3.generateWireMap(
        List(
          WireInstruction(WireDirections.Up, 3),
          WireInstruction(WireDirections.Left, 2)
        )
      ) shouldBe Map(
        (0, 1) -> NonEmptyList.one(1),
        (0, 2) -> NonEmptyList.one(2),
        (0, 3) -> NonEmptyList.one(3),
        (-1, 3) -> NonEmptyList.one(4),
        (-2, 3) -> NonEmptyList.one(5)
      )
    }
  }

  "findIntersections" - {
    "Should return no coordinates" - {
      "When there are no intersections" in {
        Day3.findIntersections(
          Map(
            (0, 1) -> NonEmptyList.one(1),
            (0, 2) -> NonEmptyList.one(2),
            (0, 3) -> NonEmptyList.one(3),
            (-1, 3) -> NonEmptyList.one(4),
            (-2, 3) -> NonEmptyList.one(5)
          )
        ) shouldBe Map.empty
      }
    }
    "Should return a map of intersections" - {
      "When there are intersections" in {
        Day3.findIntersections(
          Map(
            (0, 1) -> NonEmptyList.one(1),
            (0, 2) -> NonEmptyList.one(2),
            (0, 3) -> NonEmptyList.of(3, 1),
            (-1, 3) -> NonEmptyList.one(4),
            (-2, 3) -> NonEmptyList.one(5)
          )
        ) shouldBe Map((0, 3) -> NonEmptyList.of(3, 1))
      }
    }
  }

  "findClosestDistanceIntersection" - {
    "Should return 159 for test input" in {

      val input = """R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin

      val parsedInput: List[List[WireInstruction]] =
        input.linesIterator.toList.map(_.split(",").flatMap(parseInput).toList)

      val wire1: List[WireInstruction] = parsedInput(0)
      val wire2: List[WireInstruction] = parsedInput(1)

      Day3.findClosestDistanceIntersection(wire1, wire2) shouldBe 159
    }
    "Should return 135 for test input" in {

      val input = """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                    |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin

      val parsedInput: List[List[WireInstruction]] =
        input.linesIterator.toList.map(_.split(",").flatMap(parseInput).toList)

      val wire1: List[WireInstruction] = parsedInput(0)
      val wire2: List[WireInstruction] = parsedInput(1)

      Day3.findClosestDistanceIntersection(wire1, wire2) shouldBe 135
    }
  }

  "findLeastStepsIntersection" - {
    "Should return 610 for test input" in {

      val input = """R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin

      val parsedInput: List[List[WireInstruction]] =
        input.linesIterator.toList.map(_.split(",").flatMap(parseInput).toList)

      val wire1: List[WireInstruction] = parsedInput(0)
      val wire2: List[WireInstruction] = parsedInput(1)

      Day3.findLeastStepsIntersection(wire1, wire2) shouldBe 610
    }
    "Should return 410 for test input" in {

      val input = """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                    |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin

      val parsedInput: List[List[WireInstruction]] =
        input.linesIterator.toList.map(_.split(",").flatMap(parseInput).toList)

      val wire1: List[WireInstruction] = parsedInput(0)
      val wire2: List[WireInstruction] = parsedInput(1)

      Day3.findLeastStepsIntersection(wire1, wire2) shouldBe 410
    }
  }
}
