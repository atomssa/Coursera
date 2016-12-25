import exprs._

object Exprs {

  def main(args: Array[String]) = {
    println(Number(1))
    println(Sum(Number(1),Number(2)))
    val e = Prod(Sum(Number(1), Number(2)), Number(3))
    println(e.show)
    val e2 = Prod(Number(9), Number(5))
    println(e2.show)
  }
}
