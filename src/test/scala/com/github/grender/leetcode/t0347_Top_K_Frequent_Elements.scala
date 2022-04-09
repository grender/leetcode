import munit.FunSuite

package t0347_Top_K_Frequent_Elements {

  /*
Given an integer array nums and an integer k, return the k most frequent elements. You may return the answer in any order.



Example 1:

Input: nums = [1,1,1,2,2,3], k = 2
Output: [1,2]

Example 2:

Input: nums = [1], k = 1
Output: [1]



Constraints:

    1 <= nums.length <= 105
    k is in the range [1, the number of unique elements in the array].
    It is guaranteed that the answer is unique.



Follow up: Your algorithm's time complexity must be better than O(n log n), where n is the array's size.

   */
  object Solution {
    val reversOrdering = scala.math.Ordering.Int.reverse

    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {

      val freqs = collection.mutable.HashMap.empty[Int, Int]
      nums.foreach { i =>
        freqs(i) = freqs.getOrElse(i, 0) + 1
      }

      freqs.toList.sortBy(_._2)(reversOrdering).take(k).map(_._1).toArray
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (List(1, 1, 1, 2, 2, 3), 2) -> List(1, 2)
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.topKFrequent(arg1.toArray, arg2).toList, result.toList)
      }
    }

  }

}
