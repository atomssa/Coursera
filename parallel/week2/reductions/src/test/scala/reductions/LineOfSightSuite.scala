package reductions

import java.util.concurrent._

import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

import scala.util.Random

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

//  test("???") {
//    val input = Array[Float](0f, 1f, 8f, 9f, 5f)
//    val tree = upsweep(input, 0, 5, 1)
//    assert(tree == Leaf(1,2,3) )
//  }

//    test("???") {
//      val output = new Array[Float](5)
//      val input = Array[Float](0f, 5f, 8f, 15f, 5f)
//      downsweepSequential(input, output, 0.0f, 2, 4)
//      println(s"output= ${output.mkString(",")}")
//      assert(output.toList == List())
//    }


  test("parLineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 2)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepParallel should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    val input = Array(0f, 1f, 8f, 9f)
    val tree = upsweep(input, 0, input.length, 4)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweeSequential and downsweepParallel should give the same result") {
    val n = 8
    val npar = 4
    val outputSeq = new Array[Float](n)
    val outputPar = new Array[Float](n)
    val input = Seq.fill(n)(100.0f * Random.nextFloat).toArray
    //val input = (0 until n).map(_ % 100 * 1.0f).toArray

    println(s"inputArray= ${input.mkString(",")}")

    downsweepSequential(input, outputSeq, 0f, 0, n)
    println(s"outputSeq= ${outputSeq.mkString(",")}")

    downsweep(input, outputPar, 0f, upsweep(input, 0, n, n/npar))
    //parLineOfSight(input, outputPar, n/npar)
    println(s"outputPar= ${outputPar.mkString(",")}")


    assert(outputSeq.toList == outputPar.toList)
  }


}

