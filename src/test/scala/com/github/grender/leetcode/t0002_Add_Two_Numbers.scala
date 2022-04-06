import munit.FunSuite

/*
You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order, and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.
 */
package t0002_Add_Two_Numbers {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int         = _x
  }

  object Solution {
    case class MyListNode(var x: Int = 0, var next: MyListNode = null)

    def rr(head: MyListNode, tail: MyListNode, add: Int, l1: ListNode, l2: ListNode): MyListNode = {
      if (l1 == null && l2 == null) {
        if (add == 0) {
          return head
        } else {
          val newNode = new MyListNode(add, null)
          tail.next = newNode;
          return head;
        }
      } else if (l2 == null) {
        val sum       = l1.x + add
        val addToCur  = sum % 10
        val addToNext = sum / 10
        val newNode   = new MyListNode(addToCur)
        if (tail == null) {
          return rr(newNode, newNode, addToNext, l1.next, null)
        } else {
          tail.next = newNode;
          return rr(head, newNode, addToNext, l1.next, null)
        }
      } else if (l1 == null) {

        val sum       = l2.x + add
        val addToCur  = sum % 10
        val addToNext = sum / 10
        val newNode   = new MyListNode(addToCur)
        if (tail == null) {
          return rr(newNode, newNode, addToNext, null, l2.next)
        } else {
          tail.next = newNode
          return rr(head, newNode, addToNext, null, l2.next)
        }

      } else {
        val sum       = l1.x + l2.x + add
        val addToCur  = sum % 10
        val addToNext = sum / 10
        val newNode   = new MyListNode(addToCur)
        if (tail == null) {
          return rr(newNode, newNode, addToNext, l1.next, l2.next)
        } else {
          tail.next = newNode
          return rr(head, newNode, addToNext, l1.next, l2.next)
        }

      }
      return null
    }

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      var myResult = rr(null, null, 0, l1, l2)
      val values   = collection.mutable.ListBuffer.empty[Int]
      while (myResult != null) {
        values.addOne(myResult.x)
        myResult = myResult.next
      }
      println(values.toList)
      values
        .reverse
        .foldLeft(Option.empty[ListNode]) {
          case (None, i)        => Some(new ListNode(i))
          case (Some(oldLn), i) => Some(new ListNode(i, oldLn))
        }
        .getOrElse(null)
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (243, 564) -> (243 + 564)
    )

    def intToLn(i: Int) = {
      i.toString
        .split("")
        .map(_.toInt)
        .reverse
        .foldLeft(Option.empty[ListNode]) {
          case (None, i)        => Some(new ListNode(i))
          case (Some(oldLn), i) => Some(new ListNode(i, oldLn))
        }
        .get
    }

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        val a1      = intToLn(arg1)
        val a2      = intToLn(arg2)
        val values  = collection.mutable.ListBuffer.empty[Int]
        var gotList = Solution.addTwoNumbers(a1, a2)
        while (gotList != null) {
          values.addOne(gotList.x)
          gotList = gotList.next
        }
        val gotInt  = values.reverse.mkString("").toInt
        assertEquals(gotInt, result)
      }
    }

  }

}
