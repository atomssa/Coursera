package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (m1: Int, m2: Int) =>
    val min = if (m1 < m2) m1 else m2
    findMin(insert(m1, insert(m2, empty))) == min
  }

  property("gen3") = forAll { (m1: Int) =>
    isEmpty(deleteMin(insert(m1, empty)))
  }


  property("gen4") = forAll { (h: H) =>
    def isSorted(h1: H): Boolean = {
      val m1 = findMin(h1)
      val h2 = deleteMin(h1)
      isEmpty(h2) || (m1 <= findMin(h2) && isSorted(h2))
    }
    isSorted(h)
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val min = if (m1 <= m2) m1 else m2
    findMin(meld(h1, h2)) == min
  }

  property("gen6") = forAll{ (m1: Int, m2: Int) =>
    val max = if (m1 > m2) m1 else m2
    findMin(deleteMin(insert(m1, insert(m2, empty)))) == max
  }

  property("gen7") = forAll{ (m1: Int, m2: Int) =>
    val max = if (m1 > m2) m1 else m2
    findMin(deleteMin(deleteMin(insert(m2,insert(m1, insert(m1, empty)))))) == max
  }

}
