import munit.FunSuite

package t1260_Shift_2D_Grid {

  object Solution {

    def shiftGrid(grid: Array[Array[Int]], k: Int): List[List[Int]] = {
      val numRows = grid.size
      val numCols = grid.headOption.getOrElse(Array()).size
      val result  = Array.fill(numRows)(Array.fill(numCols)(0))
      for (idx <- 0 until numRows * numCols) {
        val row = idx / numCols
        val col = idx % numCols

        val idxN = (idx + k) % (numRows * numCols)
        val rowN = idxN / numCols
        val colN = idxN      % numCols
        result(rowN)(colN) = grid(row)(col)
      }
      result.map(_.toList).toList
    }

  }

  class Check extends FunSuite {

    val cases = List(
      (List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), 1)                   -> List(List(9, 1, 2), List(3, 4, 5), List(6, 7, 8)),
      (List(List(1), List(2), List(3), List(4), List(5), List(6), List(7)), 7) -> List(List(1), List(2), List(3), List(4), List(5), List(6), List(7))
    )

    cases.foreach { case ((arg1, arg2), result) =>
      test(s"checking $arg1 , $arg2 = $result") {
        assertEquals(Solution.shiftGrid(arg1.map(_.toArray).toArray, arg2).toString, result.toList.toString)
      }
    }

  }

}
