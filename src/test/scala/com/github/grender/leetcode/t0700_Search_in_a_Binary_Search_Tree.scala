import munit.FunSuite

package t0700_Search_in_a_Binary_Search_Tree {

  /*
You are given the root of a binary search tree (BST) and an integer val.

Find the node in the BST that the node's value equals val and return the subtree rooted with that node. If such a node does not exist, return null.
   */

  case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int      = _value
    var left: TreeNode  = _left
    var right: TreeNode = _right
  }

  object Solution {

    def searchBST(root: TreeNode, `val`: Int): TreeNode = {

      val stack = collection.mutable.Stack.empty[TreeNode]
      stack.push(root)
      while (!stack.isEmpty) {
        val p = stack.pop()
        if (p.value == `val`) {
          return p
        }

        if (p.left != null)
          stack.push(p.left)
        if (p.right != null)
          stack.push(p.right)
      }
      return null
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (
        new TreeNode(
          4,
          new TreeNode(2, TreeNode(1), TreeNode(3)),
          new TreeNode(7)
        ),
        2)  -> new TreeNode(2, TreeNode(1), TreeNode(3)),
      (
        new TreeNode(
          4,
          new TreeNode(2, TreeNode(1), TreeNode(3)),
          new TreeNode(7)
        ),
        5)  -> null,
      (
        new TreeNode(
          18,
          new TreeNode(2),
          new TreeNode(22, null, new TreeNode(63, null, new TreeNode(84)))
        ),
        63) -> new TreeNode(63, null, new TreeNode(84))
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.searchBST(arg1, arg2), result)
      }
    }

  }

}
