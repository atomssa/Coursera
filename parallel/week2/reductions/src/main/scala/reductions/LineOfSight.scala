package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var i = 1
    output(0) = 0;
    while ( i < input.length) {
      output(i) = max(output(i-1), input(i)/i)
      i = i + 1
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var i = from
    var max: Float = 0f
    while ( i < until) {
      val a = if (i==0) 0 else input(i)/i
      if (a > max) max = a
      i = i + 1
    }
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if ( (end - from) <= threshold) {
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      val mid = (from + end) /2
      val (l, r) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(l, r)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if (from < until) {
      output(from) = max(startingAngle, if (from!=0) input(from)/from else 0)
      var i = from + 1
      while ( i < until) {
        output(i) = max(output(i-1), input(i)/i)
        i = i + 1
      }
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match {
      case Leaf(f, u, _) => downsweepSequential(input, output, startingAngle, f, u)
      case Node(l, r) => {
        parallel(downsweep(input, output, startingAngle, l), downsweep(input, output, max(startingAngle, l.maxPrevious), r))
      }
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    downsweep(input, output, 0.0f, upsweep(input, 0, input.length, threshold))
  }
}

/*

[Test Description] downsweep should correctly handle trees with a single leaf
[Observed Error] List(0.0, 12.0, 12.0, 12.0, 12.0) did not equal List(0.0, 7.0, 7.0, 11.0, 12.0)
[Lost Points] 1

[Test Description] parLineOfSight should invoke the parallel construct 30 times (15 times during upsweep and 15 times during downsweep) for an array of size 17, with threshold 1
[Observed Error] 32 did not equal 30 (The number of parallel calls should be 30)
[Lost Points] 3

[Test Description] downsweepSequential should correctly handle empty chunks. E.g. if from >= end, downsweepSequential should not write anything into the output array
[Observed Error] List(0.0, 0.0, 0.0, 0.0, 12.0) did not equal List(0.0, 0.0, 0.0, 0.0, 0.0)
[Lost Points] 1

[Test Description] upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 2
[Observed Error] Node(Leaf(1,3,0.0),Leaf(3,5,7.0)) did not equal Node(Leaf(1,3,7.0),Leaf(3,5,12.0))
[Lost Points] 2

[Test Description] downsweep should correctly compute the output for a tree with 4 leaves when the starting angle is zero
[Observed Error] List(0.0, 7.0, 5.0, 11.0, 12.0) did not equal List(0.0, 7.0, 7.0, 11.0, 12.0)
[Lost Points] 2

[Test Description] downsweep should correctly compute the output for a non-zero starting angle
[Observed Error] List(0.0, 7.0, 5.0, 11.0, 12.0) did not equal List(0.0, 8.0, 8.0, 11.0, 12.0)
[Lost Points] 2

[Test Description] parLineOfSight should call parallel constuct 6 times, where the last two parallel constructs should update the 4 sections of the array (1 until 5), (5 until 9), (9 until 13), (13 until 17), respectively
[Observed Error] success was false [During the execution of first part of 5th call to parallel construct, the indices 1 until 5 of the output array was not correctly updated]
[Lost Points] 6

[Test Description] upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 1
[Observed Error] Node(Node(Leaf(1,2,0.0),Leaf(2,3,7.0)),Node(Leaf(3,4,7.0),Leaf(4,5,11.0))) did not equal Node(Node(Leaf(1,2,7.0),Leaf(2,3,5.0)),Node(Leaf(3,4,11.0),Leaf(4,5,12.0)))
[Lost Points] 2
*/