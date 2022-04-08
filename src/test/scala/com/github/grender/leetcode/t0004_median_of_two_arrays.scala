import munit.FunSuite

package t0004_median_of_two_arrays {

  /*
  Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.

The overall run time complexity should be O(log (m+n)).
   */
  object Solution {

    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

      val countElements = nums1.length + nums2.length
      val isOdd         = countElements % 2 == 1

      val stopElem = if (isOdd) {
        (countElements - 1) / 2
      } else {
        countElements / 2 - 1
      }

      def rec(idx1: Int, idx2: Int, prevPrevVal: Int = Int.MinValue, prevVal: Int = Int.MinValue): (Int, Int) = {

        val prevValIdx = idx1 + idx2 - 1
        if (prevValIdx == stopElem && isOdd) {
          (prevVal, 0)
        } else if (prevValIdx == stopElem + 1 && !isOdd) {
          return (prevPrevVal, prevVal)
        } else {
          val canGo1 = idx1 < nums1.size
          val canGo2 = idx2 < nums2.size

          if (canGo1 && canGo2) {
            val v1 = nums1(idx1)
            val v2 = nums2(idx2)
            if (v1 > v2) {
              rec(idx1, idx2 + 1, prevVal, v2)
            } else {
              rec(idx1 + 1, idx2, prevVal, v1)
            }
          } else if (!canGo1) {
            rec(idx1, idx2 + 1, prevVal, nums2(idx2))
          } else if (!canGo2) {
            rec(idx1 + 1, idx2, prevVal, nums1(idx1))
          } else {
            return (0, 0)
          }

        }
      }

      val (elA, elB) = rec(0, 0)

      if (isOdd)
        elA.toDouble
      else
        (elA.toDouble + elB.toDouble) / 2.0
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (List(1, 3), List(2))             -> 2.0,
      (List(1, 2), List(3, 4))          -> 2.5,
      (List(1, 2), List(1, 1))          -> 1.0,
      (List(1, 2), List(-1, 3))         -> 1.5,
      (List(0, 0), List(0, 0))          -> 0.0,
      (List(), List(1))                 -> 1.0,
      (List(2, 2, 2), List(2, 2, 2, 2)) -> 2.0,
      (List(1), List(2, 3))             -> 2.0
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.findMedianSortedArrays(arg1.toArray, arg2.toArray), result)
      }
    }

  }

}
