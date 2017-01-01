import exprs._

Number(1)

Number(2)

val e = Prod(Sum(Number(1), Number(2)), Number(3))
e.toString + " = " + e.eval

val e2 = Prod(Number(9), Number(5))
e2.toString + " = " + e2.eval


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (xn, acc) => f(xn) :: acc )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (xn, acc) => acc + 1 )

val l1 = List[Int](1, 2, 3, 4, 10)

mapFun[Int,Int](l1, x => x * 2 )

mapFun[Int,String](l1, x => s"a$x")

lengthFun(l1)

1 until 10


def isPrime(x: Int): Boolean = (2 until x) forall ( x % _ != 0)

isPrime(3)
isPrime(4)
isPrime(10)
isPrime(17)

def sumPrimePairs(n: Int): Seq[(Int,Int)] = {
  ((1 until n) flatMap (i => (1 until i) map (j => (i, j)))).filter(a=>isPrime(a._1 + a._2))
}

sumPrimePairs(4)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ( (x, y) <- xs zip ys ) yield x * y).sum

scalarProduct(List(3,4), List(3,4))
