import munit.FunSuite

/*
Given the head of a singly linked list, reverse the list, and return the reversed list.
 */

package t0206_Reverse_Linked_List {

  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    val next: ListNode = _next
    val x: Int         = _x
  }

  object Solution {

    def reverseList(head: ListNode): ListNode = {

      def rec(list: ListNode, result: ListNode = null): ListNode = if (list == null) {
        result
      } else {
        rec(list.next, ListNode(list.x, result))
      }

      rec(head)
    }

  }

  class Check extends FunSuite {

    def createListNode(list: List[Int]) = list
      .reverse
      .foldLeft(null.asInstanceOf[ListNode]) { (acc, elem) =>
        ListNode(elem, acc)
      }

    val cases = List(
      createListNode(List(1, 2, 3)) -> createListNode(List(3, 2, 1)),
      createListNode(List(1))       -> createListNode(List(1)),
      createListNode(List(1, 2))    -> createListNode(List(2, 1))
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {
        assertEquals(Solution.reverseList(arg), result)
      }
    }

  }

}
