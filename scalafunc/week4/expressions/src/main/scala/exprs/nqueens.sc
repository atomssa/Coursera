def nQueens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, qs: List[Int]): Boolean = {
    val row = qs.length
    val qwr = (row-1 to 0 by -1) zip qs
    qwr forall {
      case (r, c) =>
        c != col && math.abs(col - c) != math.abs(row - r)
    }
  }
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        c <- 0 until n
        if isSafe(c, queens)
      } yield c :: queens
  }
  placeQueens(n)
}
def showQueens(xs: List[Int]) = {
  val l = xs.length
  val c = for (x <- xs) yield {
    val r = for (y <- 0 until l) yield {
      val c = if (y==x) "x " else "o "
      if ( y == l-1 ) s"$c\n" else c
    }
    r.mkString
  }
  c.mkString
}

"\n" + nQueens(8).map(showQueens).mkString("\n")
