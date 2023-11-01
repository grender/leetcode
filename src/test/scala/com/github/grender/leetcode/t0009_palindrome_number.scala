import munit.FunSuite

package t0009_palindrome_number {

  object Solution {

    def isPalindrome(x: Int): Boolean = {
      val s = x.toString
      val l = s.size
      l match {
        case 1 => true
        case _ => {
          for (left <- 0 until l) {
            val right = l - 1 - left
            if (right < left) {
              return true
            }
            if (s(left) != s(right)) {
              return false
            }

          }
          return true
        }
      }
    }

  }

  class Check extends FunSuite {

    val cases = List(
      121     -> true,
      -121    -> false,
      11      -> true,
      -11     -> false,
      5       -> true,
      0       -> true,
      12321   -> true,
      1232    -> false,
      1000021 -> false
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {
        assertEquals(Solution.isPalindrome(arg), result)
      }
    }

  }

}
