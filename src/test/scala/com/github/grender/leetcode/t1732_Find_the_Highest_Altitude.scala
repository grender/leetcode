import munit.FunSuite

/*
There is a biker going on a road trip. The road trip consists of n + 1 points at different altitudes. The biker starts his trip on point 0 with altitude equal 0.

You are given an integer array gain of length n where gain[i] is the net gain in altitude between points i​​​​​​ and i + 1 for all (0 <= i < n). Return the highest altitude of a point.
 */
package t1732_Find_the_Highest_Altitude {

  object Solution {
    case class FoldAcc(maxAlt: Int, alt: Int)

    def largestAltitude(gain: Array[Int]): Int = {
      gain
        .foldLeft(FoldAcc(0, 0)) { (acc, change) =>
          val newHeight = acc.alt + change
          FoldAcc(Math.max(acc.maxAlt, newHeight), newHeight)
        }
        .maxAlt
    }

  }

  class Check extends FunSuite {

    val cases = List(
      List(-5, 1, 5, 0, -7)         -> 1,
      List(-4, -3, -2, -1, 4, 3, 2) -> 0
    )

    cases.foreach { case (arg1, result) =>
      test(s"checking $arg1 = $result") {
        assertEquals(Solution.largestAltitude(arg1.toArray), result)
      }
    }

  }

}
