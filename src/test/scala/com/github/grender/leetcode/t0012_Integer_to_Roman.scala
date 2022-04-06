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

Given an integer, convert it to a roman numeral.
 */
package t0012_Integer_to_Roman {

  object Solution {

    def intToRoman(num: Int): String = {
      val onesByIdx     = Array("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
      val tensByIdx     = Array("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
      val hundredsByIdx = Array("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
      val thousandByIdx = Array("", "M", "MM", "MMM")

      val ones     = num        % 10
      val tens     = num / 10   % 10
      val hundred  = num / 100  % 10
      val thousand = num / 1000 % 10

      thousandByIdx(thousand) + hundredsByIdx(hundred) + tensByIdx(tens) + onesByIdx(ones)
    }

  }

  class Check extends FunSuite {

    val cases = List(
      3    -> "III",
      58   -> "LVIII",
      1994 -> "MCMXCIV"
    )

    cases.foreach { case (arg1, result) =>
      test(s"checking $arg1 = $result") {
        assertEquals(Solution.intToRoman(arg1), result)
      }
    }

  }

}
