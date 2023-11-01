import munit.FunSuite

/*
Given the head of a singly linked list and two integers left and right where left <= right, reverse the nodes of the list from position left to position right, and return the reversed list.
 */

package t0092_Reverse_Linked_List_II {

  case class ListNode(var x: Int = 0, var next: ListNode = null)

  object Solution {

    def reverseBetween(headA: ListNode, left: Int, right: Int): ListNode = {

      def rec(
        current: ListNode,
        pos: Int = 1,
        preLeftElem: ListNode = null,
        newLastInside: ListNode = null,
        prevInsideElem: ListNode = null,
        head: ListNode): ListNode = if (current == null) {
        head
      } else {
        if (pos < left - 1) {
          // just passing by
          rec(current.next, pos + 1, head = head)
        } else if (pos == left - 1) {
          // save last pre left elem
          rec(current.next, pos + 1, current, head = head)
        } else if (pos == left) {
          // save newLastInside
          rec(current.next, pos + 1, preLeftElem, current, current, head = head)
        } else if (pos > left && pos < right) {
          val next = current.next
          current.next = prevInsideElem
          rec(next, pos + 1, preLeftElem, newLastInside, current, head = head)
        } else {
          val next = current.next
          current.next = prevInsideElem
          newLastInside.next = next
          if (preLeftElem == null) {
            rec(null, pos + 1, head = current)
          } else {
            preLeftElem.next = current
            rec(null, pos + 1, head = head)
          }
        }
      }

      if (left == right) {
        headA
      } else {
        rec(headA, head = headA)
      }
    }

  }

  object Implicits {

    implicit class InteratorListNode(val l: ListNode) {

      def iterator = new Iterator[Int] {

        var curr = ListNode(-1, l)

        val map = collection.mutable.Set.empty[Int]

        var counter = 0

        override def hasNext: Boolean = curr.next != null && counter < 10

        override def next(): Int = {
          curr = curr.next

          if (map.exists(_ == curr.x)) {
            counter = counter + 1
          } else {
            map.add(curr.x)
          }

          return curr.x
        }

      }

      def iteratorDescribe = new Iterator[String] {

        var curr = ListNode(-1, l)

        val map = collection.mutable.Set.empty[Int]

        var counter = 0

        override def hasNext: Boolean = curr.next != null && counter < 10

        override def next(): String = {
          curr = curr.next

          if (map.exists(_ == curr.x)) {
            counter = counter + 1
          } else {
            map.add(curr.x)
          }

          val nextX = Option(curr.next).map(_.x.toString).getOrElse("none")

          return s"(${curr.x} -> ${nextX}"
        }

      }

    }

  }

  class Check extends FunSuite {
    import Implicits._

    def createListNode(list: List[Int]) = list
      .reverse
      .foldLeft(null.asInstanceOf[ListNode]) { (acc, elem) =>
        ListNode(elem, acc)
      }

    val cases = List(
      (createListNode(List(1, 2, 3)), 2, 3)                   -> createListNode(List(1, 3, 2)),
      (createListNode(List(1, 2, 3)), 2, 2)                   -> createListNode(List(1, 2, 3)),
      (createListNode(List(1, 2, 3)), 1, 3)                   -> createListNode(List(3, 2, 1)),
      (createListNode(List(1)), 1, 1)                         -> createListNode(List(1)),
      (createListNode(List(1, 2, 3, 4, 5, 6, 7, 8, 9)), 4, 7) -> createListNode(List(1, 2, 3, 7, 6, 5, 4, 8, 9)),
      (createListNode(List(1, 2, 3, 4, 5, 6, 7, 8, 9)), 1, 5) -> createListNode(List(5, 4, 3, 2, 1, 6, 7, 8, 9))
    )

    cases.foreach { case ((arg1, arg2, arg3), result) =>
      test(s"checking $arg1 ($arg2,$arg3) = $result") {
        assertEquals(Solution.reverseBetween(arg1, arg2, arg3).iterator.toList, result.iterator.toList)
      }
    }

  }

}
