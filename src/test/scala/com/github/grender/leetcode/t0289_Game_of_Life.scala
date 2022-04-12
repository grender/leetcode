import munit.FunSuite

package t0289_Game_of_Life {

  /*
According to Wikipedia's article: "The Game of Life, also known simply as Life, is a cellular automaton devised by the British mathematician John Horton Conway in 1970."

The board is made up of an m x n grid of cells, where each cell has an initial state: live (represented by a 1) or dead (represented by a 0). Each cell interacts with its eight neighbors (horizontal, vertical, diagonal) using the following four rules (taken from the above Wikipedia article):

    Any live cell with fewer than two live neighbors dies as if caused by under-population.
    Any live cell with two or three live neighbors lives on to the next generation.
    Any live cell with more than three live neighbors dies, as if by over-population.
    Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

The next state is created by applying the above rules simultaneously to every cell in the current state, where births and deaths occur simultaneously. Given the current state of the m x n grid board, return the next state.
   */
  object Solution {

    def getNeibhorNumsInner(board: Array[Array[Int]], row: Int, col: Int): Int = {
      List(
        board(row - 1)(col - 1) % 10,
        board(row - 1)(col)     % 10,
        board(row - 1)(col + 1) % 10,
        board(row)(col - 1)     % 10,
        board(row)(col + 1)     % 10,
        board(row + 1)(col - 1) % 10,
        board(row + 1)(col)     % 10,
        board(row + 1)(col + 1) % 10
      ).sum
    }

    def getNeibhorNums(board: Array[Array[Int]], row: Int, col: Int): Int = {

      if (board.size == 1 && board(0).size == 1) {
        0
      } else if (board.size == 1 && col == 0) {
        board(row)(col + 1)
      } else if (board.size == 1 && col == board(row).size - 1) {
        board(row)(col - 1)
      } else if (board.size == 1) {
        List(
          board(row)(col - 1),
          board(row)(col + 1)
        ).sum
      } else if (board(row).size == 1 && row == 0) {
        board(row + 1)(col)
      } else if (board(row).size == 1 && row == board.size - 1) {
        board(row - 1)(col)
      } else if (board(row).size == 1) {
        List(
          board(row - 1)(col),
          board(row + 1)(col)
        ).sum

      } else if (row == 0 && col == 0) {
        List(
          board(row)(col + 1)     % 10,
          board(row + 1)(col)     % 10,
          board(row + 1)(col + 1) % 10
        ).sum
      } else if (row == 0 && col == board(row).size - 1) {
        List(
          board(row)(col - 1)     % 10,
          board(row + 1)(col - 1) % 10,
          board(row + 1)(col)     % 10
        ).sum
      } else if (row == 0) {
        List(
          board(row)(col - 1)     % 10,
          board(row)(col + 1)     % 10,
          board(row + 1)(col - 1) % 10,
          board(row + 1)(col)     % 10,
          board(row + 1)(col + 1) % 10
        ).sum
      } else if (row == board.size - 1 && col == 0) {
        List(
          board(row)(col + 1)     % 10,
          board(row - 1)(col)     % 10,
          board(row - 1)(col + 1) % 10
        ).sum
      } else if (row == board.size - 1 && col == board(row).size - 1) {
        List(
          board(row)(col - 1)     % 10,
          board(row - 1)(col - 1) % 10,
          board(row - 1)(col)     % 10
        ).sum
      } else if (row == board.size - 1) {
        List(
          board(row)(col - 1)     % 10,
          board(row)(col + 1)     % 10,
          board(row - 1)(col - 1) % 10,
          board(row - 1)(col)     % 10,
          board(row - 1)(col + 1) % 10
        ).sum
      } else if (col == 0) {
        List(
          board(row - 1)(col)     % 10,
          board(row - 1)(col + 1) % 10,
          board(row)(col + 1)     % 10,
          board(row + 1)(col)     % 10,
          board(row + 1)(col + 1) % 10
        ).sum
      } else if (col == board(row).size - 1) {
        List(
          board(row - 1)(col - 1) % 10,
          board(row - 1)(col)     % 10,
          board(row)(col - 1)     % 10,
          board(row + 1)(col - 1) % 10,
          board(row + 1)(col)     % 10
        ).sum
      } else {
        ???
      }
    }

    def gameOfLife(board: Array[Array[Int]]): Unit = {

      for (row <- 1 until board.size - 1) {
        for (col <- 1 until board(row).size - 1) {
          val neighborsNums = getNeibhorNumsInner(board, row, col)
          val currentValue  = board(row)(col)
          board(row)(col) = currentValue + neighborsNums * 10
        }
      }

      for (row <- 0 until board.size) {
        val neighborsNums = getNeibhorNums(board, row, 0)
        val currentValue  = board(row)(0)
        board(row)(0) = currentValue + neighborsNums * 10

        val neighborsNums2 = getNeibhorNums(board, row, board(row).size - 1)
        val currentValue2  = board(row)(board(row).size - 1)
        board(row)(board(row).size - 1) = currentValue2 + neighborsNums2 * 10
      }

      for (col <- 1 until board(0).size - 1) {
        val neighborsNums = getNeibhorNums(board, 0, col)
        val currentValue  = board(0)(col)
        board(0)(col) = currentValue + neighborsNums * 10

        val neighborsNums2 = getNeibhorNums(board, board.size - 1, col)
        val currentValue2  = board(board.size - 1)(col)
        board(board.size - 1)(col) = currentValue2 + neighborsNums2 * 10
      }

      for (row <- 0 until board.size) {
        for (col <- 0 until board(row).size) {
          val currentValue = board(row)(col)
          board(row)(col) = if (currentValue % 10 == 1) {
            val neighborsNums = currentValue / 10
            if (neighborsNums < 2) {
              0
            } else if (neighborsNums == 2 || neighborsNums == 3) {
              1
            } else {
              0
            }
          } else {
            val neighborsNums = currentValue / 10
            if (neighborsNums == 3) {
              1
            } else {
              0
            }
          }
        }
      }

    }

  }

  class Check extends FunSuite {

    val cases = List(
      "1,1|1,0"                 -> "1,1|1,1",
      "0,1,0|0,0,1|1,1,1|0,0,0" -> "0,0,0|1,0,1|0,1,1|0,1,0",
      "0"                       -> "0",
      "0,0"                     -> "0,0",
      "0|0"                     -> "0|0",
      "1,0,0,1"                 -> "0,0,0,0"
    )

    cases.foreach { case (arg, result) =>
      test(s"checking $arg = $result") {

        def toArr(s: String) = s.split("\\|").map(_.split(",").map(_.toInt))

        val argArr = toArr(arg)
        Solution.gameOfLife(argArr)
        assertEquals(argArr.toList.map(_.toList).toString(), toArr(result).toList.map(_.toList).toString())

      }
    }

  }

}
