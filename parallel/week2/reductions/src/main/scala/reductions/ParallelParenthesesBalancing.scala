package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceAcc(chars: Array[Char], pos: Int, acc: Int): Boolean = {
      if (pos == chars.length) {
        acc == 0
      } else if ( acc < 0 ) false
      else if ( chars(pos) == '(' ) {
        balanceAcc(chars, pos+1, acc+1)
      } else if ( chars(pos) == ')') {
        balanceAcc(chars, pos+1, acc-1)
      } else balanceAcc(chars, pos + 1, acc)
    }
    balanceAcc(chars, 0, 0)
  }

//  /** Returns `true` iff the parentheses in the input `chars` are balanced.
//    */
//  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
//
//    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
//      ???
//    }
//
//    def reduce(from: Int, until: Int) /*: ???*/ = {
//      ???
//    }
//
//    reduce(0, chars.length) == 0
//  }

//  def balance(chars: Array[Char]) = parBalance(chars, 3)

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, acc: Int, arg2: Int) : (Int, Int) = {
      var pos = idx
      var tot = 0
      var min = 0
      while (pos < until) {
        if ( chars(pos) == '(' ) {
          tot = tot + 1
        }
        if ( chars(pos) == ')') {
          tot = tot - 1
        }
        if (tot < min) min = tot
        pos = pos + 1
      }
      (tot, min)
    }
    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) {
        val (tot, min) = traverse(from, until, 0, 0)
        (tot, min)
      } else {
        val mid = (until + from) / 2
        val (a, b) = parallel(reduce(from, mid), reduce(mid, until))
        (a._1 + b._1, Math.min(a._2, a._1 + b._2) )
      }
    }
    val v = reduce(0, chars.length)
    v._1 == 0 && v._2 >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
