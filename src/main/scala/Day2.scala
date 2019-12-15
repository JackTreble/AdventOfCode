import cats.implicits._
import enumeratum.values._
object Day2 {

  def main(args: Array[String]): Unit = {

    val input = List(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1,
      19, 1, 19, 5, 23, 2, 9, 23, 27, 1, 5, 27, 31, 1, 5, 31, 35, 1, 35, 13, 39,
      1, 39, 9, 43, 1, 5, 43, 47, 1, 47, 6, 51, 1, 51, 13, 55, 1, 55, 9, 59, 1,
      59, 13, 63, 2, 63, 13, 67, 1, 67, 10, 71, 1, 71, 6, 75, 2, 10, 75, 79, 2,
      10, 79, 83, 1, 5, 83, 87, 2, 6, 87, 91, 1, 91, 6, 95, 1, 95, 13, 99, 2,
      99, 13, 103, 1, 103, 9, 107, 1, 10, 107, 111, 2, 111, 13, 115, 1, 10, 115,
      119, 1, 10, 119, 123, 2, 13, 123, 127, 2, 6, 127, 131, 1, 13, 131, 135, 1,
      135, 2, 139, 1, 139, 6, 0, 99, 2, 0, 14, 0)

    val computedInput = input.updated(1, 12).updated(2, 2)

    val computedIntcode = computeIntcode(computedInput)

    println(
      s"computed IntCode [$computedIntcode] - answer = [${computedIntcode.head}]"
    )

    for (noun <- 0 to 99;
         verb <- 0 to 99;
         updated = input.updated(1, noun).updated(2, verb)
         if computeIntcode(updated).headOption.contains(19690720))
      yield
        println(s"noun [$noun], verb [$verb] - answer = [${100 * noun + verb}]")

  }

  type IntOperation = (Int, Int) => Int

  sealed abstract class OpCode(val value: Int, val operation: IntOperation)
      extends IntEnumEntry

  object OpCodes extends IntEnum[OpCode] {
    case object ADD extends OpCode(value = 1, operation = _ + _)
    case object MULTIPLY extends OpCode(value = 2, operation = _ * _)

    val values = findValues
  }

  def computeIntcode(intCode: List[Int]): List[Int] = {

    @scala.annotation.tailrec
    def computeIntcode(intCode: List[Int], pos: Int): List[Int] = {
      val nextPos = pos + 4
      intCode.slice(pos, nextPos) match {
        case List(opcode, in1Pos, in2Pos, store) =>
          (
            OpCodes.withValueOpt(opcode),
            intCode.get(in1Pos),
            intCode.get(in2Pos)
          ).mapN((opCode, val1, val2) => opCode.operation(val1, val2)) match {
            case Some(value) =>
              computeIntcode(intCode.updated(store, value), nextPos)
            case None => intCode
          }
        case _ => intCode
      }
    }
    computeIntcode(intCode, 0)
  }

}
