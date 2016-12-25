import exprs._

Number(1)

Number(2)

val e = Prod(Sum(Number(1), Number(2)), Number(3))
e.toString + " = " + e.eval

val e2 = Prod(Number(9), Number(5))
e2.toString + " = " + e2.eval