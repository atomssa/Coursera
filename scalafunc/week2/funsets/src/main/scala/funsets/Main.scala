package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s10 = (a: Int) => a > 0 && a < 10
  val pEven = (a: Int) => a % 2 == 0

  println("s10: " + FunSets.toString(s10) )
  val test = map(s10, (a: Int) => a * 2 )
  println("s10.map(_/2): " + FunSets.toString(test))
}
