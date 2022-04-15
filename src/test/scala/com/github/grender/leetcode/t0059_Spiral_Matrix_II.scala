import munit.FunSuite

package t0059_Spiral_Matrix_II {

  /*
Given a positive integer n, generate an n x n matrix filled with elements from 1 to n2 in spiral order.
   */
  object Solution {

    def generateMatrix(n: Int): Array[Array[Int]] = {

      if (n == 0) {
        return Array()
      }
      // dirction
      // 0 - right
      // 1 - down
      // 2 - left
      // 3 - up
      val result = Array.fill(n)(Array.fill(n)(0))

      def rec(num: Int, row: Int, col: Int, direction: Int): Unit = {
        result(row)(col) = num
        if (num == n * n) {
          ()
        } else
          direction match {
            case 0 =>
              if (col == n - 1 || result(row)(col + 1) > 0) {
                rec(num, row, col, 1)
              } else {
                rec(num + 1, row, col + 1, direction)
              }
            case 1 =>
              if (row == n - 1 || result(row + 1)(col) > 0) {
                rec(num, row, col, 2)
              } else {
                rec(num + 1, row + 1, col, direction)
              }
            case 2 =>
              if (col == 0 || result(row)(col - 1) > 0) {
                rec(num, row, col, 3)
              } else {
                rec(num + 1, row, col - 1, direction)
              }
            case 3 =>
              if (row == 0 || result(row - 1)(col) > 0) {
                rec(num, row, col, 0)
              } else {
                rec(num + 1, row - 1, col, direction)
              }
          }
      }
      rec(1, 0, 0, 0)
      result
    }

  }

  class Check extends FunSuite {

    val cases = List(
      3 -> "1,2,3|8,9,4|7,6,5",
      4 -> "1,2,3,4|12,13,14,5|11,16,15,6|10,9,8,7",
      1 -> "1",
      0 -> ""
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {

        def toArr(s: String): Array[Array[Int]] = {
          if (s == "") Array()
          else
            s.split("\\|").map(_.split(",").map(_.toInt))
        }

        assertEquals(Solution.generateMatrix(arg).toList.map(_.toList).toString(), toArr(result).toList.map(_.toList).toString())

      }
    }

  }

}
