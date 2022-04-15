import munit.FunSuite

package t0669_Trim_a_Binary_Search_Tree {

  /*
Given the root of a binary search tree and the lowest and highest boundaries as low and high, trim the tree so that all its elements lies in [low, high]. Trimming the tree should not change the relative structure of the elements that will remain in the tree (i.e., any node's descendant should remain a descendant). It can be proven that there is a unique answer.

Return the root of the trimmed binary search tree. Note that the root may change depending on the given bounds.
   */

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int      = _value
    var left: TreeNode  = _left
    var right: TreeNode = _right

    override def equals(obj: Any): Boolean = if (obj.isInstanceOf[TreeNode]) {
      val other = obj.asInstanceOf[TreeNode]
      value == other.value && left.equals(other.left) && right.equals(other.right)
    } else {
      false
    }

    override def toString(): String = {
      s"($value, L:$left, R:$right)"
    }

  }

  object Solution {

    def trimBST(root: TreeNode, low: Int, high: Int): TreeNode = {

      val range        = low to high
      var resultRoot   = root
      var elementQueue = collection.mutable.Queue.empty[TreeNode]
      var cur          = root

      while (cur != null || elementQueue.nonEmpty) {
        if (cur == null) {
          cur = elementQueue.dequeue()
        }

        if (cur.value < low) {
          cur = cur.right
          resultRoot = cur
        } else if (cur.value > high) {
          cur = cur.left
          resultRoot = cur
        } else {
          if (cur.right != null && range.contains(cur.right.value) && cur.left != null && range.contains(cur.left.value)) {
            elementQueue.enqueue(cur.right)
            elementQueue.enqueue(cur.left)
            cur = null

          } else if (cur.right != null && !range.contains(cur.right.value)) {
            cur.right = cur.right.left
          } else if (cur.left != null && !range.contains(cur.left.value)) {
            cur.left = cur.left.right
          } else if (cur.right != null && range.contains(cur.right.value)) {
            cur = cur.right
          } else if (cur.left != null && range.contains(cur.left.value)) {
            cur = cur.left
          } else {
            cur = null
          }
        }
      }

      resultRoot
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (
        new TreeNode(
          4,
          new TreeNode(2, new TreeNode(1), new TreeNode(3)),
          new TreeNode(7)
        ),
        2,
        5)  -> new TreeNode(
        4,
        new TreeNode(2, null, new TreeNode(3))
      ),
      (
        new TreeNode(
          4,
          new TreeNode(2, new TreeNode(1), new TreeNode(3)),
          new TreeNode(7)
        ),
        5,
        7)  -> new TreeNode(7),
      (
        new TreeNode(
          18,
          new TreeNode(2),
          new TreeNode(22, null, new TreeNode(63, null, new TreeNode(84)))
        ),
        18,
        44) -> new TreeNode(18, null, new TreeNode(22))
    )

    cases.foreach { case ((arg1, arg2, arg3), result) =>
      test(s"checking $arg1 , $arg2, $arg3 = $result") {
        assertEquals(Solution.trimBST(arg1, arg2, arg3).toString(), result.toString())
      }
    }

  }

}
