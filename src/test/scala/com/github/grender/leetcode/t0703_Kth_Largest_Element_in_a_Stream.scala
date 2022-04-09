import munit.FunSuite

package t0703_Kth_Largest_Element_in_a_Stream {

  /*
Design a class to find the kth largest element in a stream. Note that it is the kth largest element in the sorted order, not the kth distinct element.

Implement KthLargest class:

    KthLargest(int k, int[] nums) Initializes the object with the integer k and the stream of integers nums.
    int add(int val) Appends the integer val to the stream and returns the element representing the kth largest element in the stream.



Example 1:

Input
["KthLargest", "add", "add", "add", "add", "add"]
[[3, [4, 5, 8, 2]], [3], [5], [10], [9], [4]]
Output
[null, 4, 5, 5, 8, 8]

Explanation
KthLargest kthLargest = new KthLargest(3, [4, 5, 8, 2]);
kthLargest.add(3);   // return 4
kthLargest.add(5);   // return 5
kthLargest.add(10);  // return 5
kthLargest.add(9);   // return 8
kthLargest.add(4);   // return 8



Constraints:

    1 <= k <= 104
    0 <= nums.length <= 104
    -104 <= nums[i] <= 104
    -104 <= val <= 104
    At most 104 calls will be made to add.
    It is guaranteed that there will be at least k elements in the array when you search for the kth element.


   */

  class KthLargest(_k: Int, _nums: Array[Int]) {

    var maxElements     = _nums.sortWith(_ > _).take(_k)
    var kthMaxElement   = maxElements.lastOption.getOrElse(Int.MinValue)
    var firstMaxElement = maxElements.headOption.getOrElse(Int.MinValue)

    def add(`val`: Int): Int = {
      if (`val` > firstMaxElement) {
        maxElements = Array(`val`).concat(maxElements).take(_k)
        firstMaxElement = `val`
        kthMaxElement = maxElements.last
      } else if (`val` < kthMaxElement && maxElements.size == _k) {
        kthMaxElement
      } else {
        maxElements = Array(`val`).concat(maxElements).sortWith(_ > _).take(_k)
        firstMaxElement = maxElements.head
        kthMaxElement = maxElements.last
      }
      kthMaxElement
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (3, List(4, 5, 8, 2), List(3, 5, 10, 9, 4)) -> List(4, 5, 5, 8, 8),
      (1, List(), List(-3, -2, -4, 0, 4))         -> List(-3, -2, -2, 0, 4),
      (2, List(0), List(-1, 1, -2, -4, 3))        -> List(-1, 0, 0, 0, 1),
      (3, List(5, -1), List(2, 1, -1, 3, 4))      -> List(-1, 1, 1, 2, 3)
    )

    cases.foreach { case ((k, nums, addCalls), addResult) =>
      test(s"checking k=$k nums=$nums calls=$addCalls should be $addResult") {
        val testClass = new KthLargest(k, nums.toArray)
        addCalls
          .zip(addResult)
          .map { case (arg, result) =>
            val callResult = testClass.add(arg)
            assertEquals(callResult, result)
          }
      }
    }

  }

}
