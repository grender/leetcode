import munit.FunSuite

package t0682_Baseball_Game {

  /*
You are keeping score for a baseball game with strange rules. The game consists of several rounds, where the scores of past rounds may affect future rounds' scores.

At the beginning of the game, you start with an empty record. You are given a list of strings ops, where ops[i] is the ith operation you must apply to the record and is one of the following:

    An integer x - Record a new score of x.
    "+" - Record a new score that is the sum of the previous two scores. It is guaranteed there will always be two previous scores.
    "D" - Record a new score that is double the previous score. It is guaranteed there will always be a previous score.
    "C" - Invalidate the previous score, removing it from the record. It is guaranteed there will always be a previous score.

Return the sum of all the scores on the record.
   */
  object Solution {

    def calPoints(ops: Array[String]): Int = {

      def r(opsIdx: Int, sum: Int, valList: List[Int]): Int = {
        if (opsIdx == ops.size) {
          sum
        } else {
          ops(opsIdx) match {
            case "+" => {
              val v1     = valList(0)
              val v2     = valList(1)
              val newVal = v1 + v2
              r(opsIdx + 1, sum + newVal, newVal :: valList)
            }
            case "D" => {
              val newVal = valList(0) * 2
              r(opsIdx + 1, sum + newVal, newVal :: valList)
            }
            case "C" => {
              r(opsIdx + 1, sum - valList.head, valList.tail)
            }
            case a   => {
              val newVal = a.toInt
              r(opsIdx + 1, sum + newVal, newVal :: valList)
            }
          }
        }
      }

      r(0, 0, List())
    }

  }

  class Check extends FunSuite {

    val cases = List(
      List("5", "2", "C", "D", "+")                 -> 30,
      List("5", "-2", "4", "C", "D", "9", "+", "+") -> 27,
      List("1")                                     -> 1,
      List()                                        -> 0
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {
        assertEquals(Solution.calPoints(arg.toArray), result)
      }
    }

  }

}
