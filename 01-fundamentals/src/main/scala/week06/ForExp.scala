package week06

package object forexp {
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  def primeSumPairs(n: Int) = {
    (1 until n) flatMap (
      i => (1 until i) map (j => (i, j)) filter (
        pair => isPrime(pair._1 + pair._2)
      )
    )
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r,c) => col != c && math.abs(col - c) != row - r
    }
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }

    placeQueens(n)
  }
}
