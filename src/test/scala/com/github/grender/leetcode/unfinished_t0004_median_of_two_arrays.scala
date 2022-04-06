import munit.FunSuite

package t0004_median_of_two_arrays {

  object Solution {

    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

      val countElements = nums1.length + nums2.length
      val isOdd         = countElements % 2 == 1

      val stopElem = if (isOdd) {
        (countElements - 1) / 2
      } else {
        countElements / 2 - 1
      }

      println(stopElem)

      def rec(curElem: Int, curIdx1: Int, curIdx2: Int, arr1complete: Boolean, arr2complete: Boolean): (Int, Int) = {
        if (curElem == stopElem) {
          (curIdx1, curIdx2)
        } else if (!arr1complete && curIdx1 == nums1.size) {
          rec(curElem, curIdx1, curIdx2, true, arr2complete)
        } else if (!arr2complete && curIdx2 == nums2.size) {
          rec(curElem, curIdx1, curIdx2, arr1complete, true)
        } else {
          (nums1(curIdx1), nums2(curIdx2)) match {
            case (n1, n2) if n2 > n1 && !arr1complete => rec(curElem + 1, curIdx1 + 1, curIdx2, arr1complete, arr2complete)
            case (n1, n2) if n2 < n1 && !arr2complete => rec(curElem + 1, curIdx1, curIdx2 + 1, arr1complete, arr2complete)
            case _ if (curIdx1 < nums1.length - 1)    => rec(curElem + 1, curIdx1 + 1, curIdx2, arr1complete, arr2complete)
            case _ if (curIdx2 < nums2.length - 1)    => rec(curElem + 1, curIdx1, curIdx2 + 1, arr1complete, arr2complete)
          }
        }
      }

      val (elA, elB) = if (nums1.length != 0 && nums2.length != 0) {

        val (idx1, idx2) = rec(0, 0, 0, false, false)

        val nextValueIdx1 = if (idx1 >= nums1.length - 1) {
          None
        } else {
          Some(nums1(idx1 + 1))
        }

        val nextValueIdx2 = if (idx2 >= nums2.length - 1) {
          None
        } else {
          Some(nums2(idx2 + 1))
        }

        val sList = (nums1(idx1), nextValueIdx1, nums2(idx2), nextValueIdx2) match {
          case (n11, Some(n12), n21, Some(n22)) => List(n11, n12, n21, n22).sorted
          /*
      if(n12<n21) (n12,n21)
      else if(n12>n21 && n12<n22) (n21,n12)
      else if(n12>n21 && n12>n22) (n21,n22)
      else if(n11==n21 && n12<n22) (n11,n12)
      else if(n11==n21 && n12>=n22) (n11,n22)
           */
          case (n11, None, n21, Some(n22))      => List(n11, n21, n22).sorted
          case (n11, None, n21, None)           => List(n11, n21).sorted
          case (n11, Some(n12), n21, None)      => List(n11, n12, n21).sorted

          // n11<=n12
          // n21<=n22
        }
        (sList(0), sList(1))
      } else if (nums1.length == 0 && isOdd)
        (nums2(stopElem), 0)
      else if (nums1.length == 0 && !isOdd)
        (nums2(stopElem), nums2(stopElem + 1))
      else if (nums2.length == 0 && isOdd)
        (nums1(stopElem), 0)
      else if (nums2.length == 0 && !isOdd)
        (nums1(stopElem), nums1(stopElem + 1))
      else (0, 0)

      if (isOdd)
        elA.toDouble
      else
        (elA.toDouble + elB.toDouble) / 2.0

    }

  }

  class Check extends FunSuite {

    val cases = List(
      // (List(1, 3), List(2))             -> 2.0,
      // (List(1, 2), List(3, 4))          -> 2.5,
      // (List(1, 2), List(1, 1))          -> 1.0,
      // (List(1, 2), List(-1, 3))         -> 1.5,
      // (List(0, 0), List(0, 0))          -> 0.0,
      // (List(), List(1))                 -> 1.0,
      // (List(2, 2, 2), List(2, 2, 2, 2)) -> 2.0,
      (List(1), List(2, 3)) -> 2.0
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.findMedianSortedArrays(arg1.toArray, arg2.toArray), result)
      }
    }

  }

}
