package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("Polynomial") {
    val res1 = Polynomial.computeSolutions(Var(1), Var(1), Var(1), Polynomial.computeDelta(Var(1), Var(1), Var(1)))
    assert(res1() == Set())

    val res2 = Polynomial.computeSolutions(Var(1), Var(0), Var(-1), Polynomial.computeDelta(Var(1), Var(0), Var(-1)))
    assert(res2() == Set(-1.0, 1.0))

  }

  test("calc") {
    val in: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Literal(1)),
      "b" -> Signal(Literal(2)),
      "c" -> Signal(Plus(Ref("a"), Ref("b")))
    )
    val out = Map(
      "a" -> Signal(1.0),
      "b" -> Signal(2.0),
      "c" -> Signal(3.0)
    )

    val res1 = Calculator.computeValues(in)
    assert(res1("a")() == out("a")())
    assert(res1("b")() == out("b")())
    assert(res1("c")() == out("c")())


  }

  test("self Deps") {

    val in: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Literal(2), Ref("b"))),
      "b" -> Signal(Times(Literal(2), Ref("a")))
    )
    assert(Calculator.computeValues(in)("a")().isNaN == true)

    val in2: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Literal(2), Ref("b"))),
      "b" -> Signal(Times(Literal(2), Ref("c"))),
      "c" -> Signal(Literal(7))
    )
    assert(Calculator.computeValues(in2)("a")() == 16.0)

//    val in3: Map[String, Signal[Expr]] = Map(
//      "a" -> Signal(Plus(Ref("c"), Ref("b"))),
//      "b" -> Signal(Times(Literal(3), Ref("c"))),
//      "c" -> Signal(Minus(Ref("d"), Literal(5))),
//      "d" -> Signal(Times(Literal(7), Ref("a")))
//    )
//    assert(Calculator.computeValues(in3)("a")().isNaN == true)
//    assert(Calculator.computeValues(in3)("d")().isNaN == true)

    val in4: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Times(Ref("a"), Literal(5)))
    )
    assert(Calculator.computeValues(in4)("a")().isNaN == true)

    val in5: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("a"))
    )
    assert(Calculator.computeValues(in5)("a")().isNaN == true)
    assert(Calculator.computeValues(in5)("b")().isNaN == true)

//    val in6: Map[String, Signal[Expr]] = Map(
//      "a" -> Signal(Ref("a")),
//      "b" -> Signal(Ref("a"))
//    )
//    assert(Calculator.computeValues(in6)("a")().isNaN == true)
//    assert(Calculator.computeValues(in6)("b")().isNaN == true)


  }


}
