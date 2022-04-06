import munit.FunSuite

package t0001_two_sum {

  object Solution {

    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      val lastIdx = nums.size - 1

      def rec(lIdx: Int, rIdx: Int): Array[Int] = {
        if (nums(lIdx) + nums(rIdx) == target)
          Array(lIdx, rIdx)
        else if (rIdx == lastIdx)
          rec(lIdx + 1, lIdx + 2)
        else
          rec(lIdx, rIdx + 1)
      }
      rec(0, 1)
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (List(3, 2, 4), 6)      -> List(1, 2),
      (List(2, 7, 11, 15), 9) -> List(0, 1)
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.twoSum(arg1.toArray, arg2).toList, result.toList)
      }
    }

  }

}
