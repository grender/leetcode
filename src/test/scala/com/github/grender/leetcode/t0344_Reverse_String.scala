import munit.FunSuite

package t0344_Reverse_String {

  object Solution {
    var t: Char = ' '

    def reverseString(s: Array[Char]): Unit = {
      for (i <- 0 until s.size / 2) {
        t = s(i)
        s(i) = s(s.size - i - 1)
        s(s.size - i - 1) = t
      }
    }

  }

  class Check extends FunSuite {

    val cases = List(
      "Hello"  -> "olleH",
      "DzA"    -> "AzD",
      "AdDaZa" -> "aZaDdA",
      "123456" -> "654321"
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {
        val newResult = arg.toArray
        Solution.reverseString(newResult)
        assertEquals(newResult.mkString, result)
      }
    }

  }

}
