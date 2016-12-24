package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton2")
      assert(contains(s3, 3), "Singleton3")
      assert(!contains(s2, 3), "Singleton4")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("Tests for function forall"){
    new TestSets {
      val s = union(s1, union(s2, s3))
      assert(forall(s2, s2), "All elements of s2 should be s2")
      assert(!forall(union(s1, s2), s3), "Not all elements of s1 U s2 are in s3")
      assert(!forall(union(s1, s2), s2), "Not all elements of s1 U s2 are in s2")
      val s10 = (a: Int) =>  a> 0 && a < 10
      val pEven = (a: Int) => a % 2 == 0
      val pOdd = (a: Int) => a % 2 != 0
      assert(!forall(s10, pEven), "Not all integers between 0 and 10 are even")
      assert(forall(union(s1, s3), pOdd), "All elements of s1 U s3 are odd")
    }
  }

  test("Tests for function exists"){
    new TestSets {
      val s = union(s1,s2)
      assert(exists(s1,s1) ,"There is an element of s1 also in s1")
      assert(!exists(s1,s2) ,"There exists no element of s1 that is in s2")
      assert(exists(s,s1) ,"There exists an element of s1 U s2 that is in s1")
      assert(!exists(s,s3) ,"There exists no element of s1 U s2 that is in s3")
    }
  }

  test("Test for function map"){
    new TestSets {
      val s10 = (a: Int) =>  a> 0 && a < 10
      val pEven = (a: Int) => a % 2 == 0
      assert(!forall(map(s10, (a : Int ) => a) , pEven ), "Not all elements of s10 are even" )
      assert(forall(map(s10, (a : Int ) => 2*a) , pEven ), "All elements of s10 mapped by f(x) = x * 2 are even" )
      assert(!forall(map(s10, (a : Int ) => 3*a) , pEven ), "All elements of s10 mapped by f(x) = x * 3 are even" )
    }
  }


}
