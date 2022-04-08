import munit.FunSuite

/*
Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.

Symbol       Value
I             1
V             5
X             10
L             50
C             100
D             500
M             1000

For example, 2 is written as II in Roman numeral, just two one's added together. 12 is written as XII, which is simply X + II. The number 27 is written as XXVII, which is XX + V + II.

Roman numerals are usually written largest to smallest from left to right. However, the numeral for four is not IIII. Instead, the number four is written as IV. Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX. There are six instances where subtraction is used:

    I can be placed before V (5) and X (10) to make 4 and 9.
    X can be placed before L (50) and C (100) to make 40 and 90.
    C can be placed before D (500) and M (1000) to make 400 and 900.

Given a roman numeral, convert it to an integer.
 */
package t0013_Roman_to_Integer {

  object Solution {

    val value = Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )

    def romanToInt(s: String): Int = {
      def r(idx: Int, subSum: Int, prevValue: Int, sum: Int): Int = {
        if (idx == s.size) {
          sum + subSum
        } else {
          val letter = s(idx)
          val int    = value(letter)
          if (int == prevValue) {
            r(idx + 1, subSum + int, int, sum)
          } else if (prevValue < int) {
            r(idx + 1, int, int, sum - subSum)
          } else {
            r(idx + 1, int, int, sum + subSum)
          }

        }
      }

      r(0, 0, 0, 0)
    }

  }

  class Check extends FunSuite {

    val cases = (1 until 3999).map { i =>
      t0012_Integer_to_Roman.Solution.intToRoman(i) -> i
    }

    cases.foreach { case (arg1, result) =>
      test(s"checking $arg1 = $result") {
        assertEquals(Solution.romanToInt(arg1), result)
      }
    }

  }

}
