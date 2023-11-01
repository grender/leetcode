import munit.FunSuite

/*
Given the head of a singly linked list, return true if it is a
palindrome
or false otherwise.
 */

package t0234_palindrome_linked_list {

  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    val next: ListNode = _next
    val x: Int         = _x
  }

  object Solution {

    def isPalindrome(head: ListNode): Boolean = {

      def count(curr: ListNode, result: Int = 0): Int = {
        if (curr == null) {
          result
        } else {
          count(curr.next, result + 1)
        }
      }

      val length         = count(head)
      val middlePosition = length / 2
      val isDividedBy2   = length % 2 == 0

      def calcImpactD(length: Int, elem: Int, position: Int) =
        (elem * (length - position + 1) + elem + (length - position + 1)) ^ (length - position + 1)
      def calcImpactU(length: Int, elem: Int, position: Int) = (elem * position + elem + position) ^ position

      def checkPolindrome(curr: ListNode, currPos: Int = 1, sum: Int = 0): Int = {
        if (curr == null) {
          sum
        } else {
          if (isDividedBy2) {
            if (currPos > middlePosition) {
              checkPolindrome(curr.next, currPos + 1, sum - calcImpactD(length, curr.x, currPos))
            } else {
              checkPolindrome(curr.next, currPos + 1, sum + calcImpactU(length, curr.x, currPos))
            }
          } else {
            if (currPos == middlePosition + 1) {
              checkPolindrome(curr.next, currPos + 1, sum)
            } else if (currPos > middlePosition) {
              checkPolindrome(curr.next, currPos + 1, sum - calcImpactD(length, curr.x, currPos))
            } else {
              checkPolindrome(curr.next, currPos + 1, sum + calcImpactU(length, curr.x, currPos))
            }
          }
        }
      }

      checkPolindrome(head) == 0
    }

  }

  class Check extends FunSuite {

    val cases = List(
      ListNode(1, ListNode(2, ListNode(1)))                                                                  -> true,
      ListNode(1, ListNode(1))                                                                               -> true,
      ListNode(-1, ListNode(1))                                                                              -> false,
      ListNode(0)                                                                                            -> true,
      ListNode(1, ListNode(2, ListNode(3, ListNode(2, ListNode(1)))))                                        -> true,
      ListNode(1, ListNode(2, ListNode(3, ListNode(1, ListNode(2)))))                                        -> false,
      ListNode(1, ListNode(2, ListNode(1, ListNode(3))))                                                     -> false,
      ListNode(2, ListNode(3, ListNode(3, ListNode(2, ListNode(3, ListNode(2, ListNode(2, ListNode(3)))))))) -> false
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {
        assertEquals(Solution.isPalindrome(arg), result)
      }
    }

  }

}

object Main extends App {
  println(scala.io.StdIn.readLine().split(" ").map(_.toInt).sum)
}
