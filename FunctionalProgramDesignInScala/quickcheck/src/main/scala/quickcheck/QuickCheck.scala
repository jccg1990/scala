package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def findAndDelete(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else {
      findMin(h) :: findAndDelete(deleteMin(h))
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = Math.min(a, b)
    findMin(h) == min
  }

  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    val max = Math.max(c, Math.max(a, b))
    findMin(deleteMin(deleteMin(h))) == max
  }

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("deleteMin2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == Math.max(a,b)
  }

  property("sortedElements") = forAll { h: H =>
    val l = findAndDelete(h)

    l == l.sorted
  }

  property("meldingMin") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) {
      meld(h1, h2) == empty
    } else if (isEmpty(h1)) {
      meld(h1, h2) == h2
    } else if (isEmpty(h2)) {
      meld(h1, h2) == h1
    } else {
      val x1 = findMin(h1)
      val x2 = findMin(h2)
      val m = meld(h1, h2)
      findMin(m) == Math.min(x1, x2)
    }
  }

  property("meldingSorted") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) {
      meld(h1, h2) == empty
    } else if (isEmpty(h1)) {
      val m = meld(h1, h2)
      val l = findAndDelete(m)

      l == l.sorted
    } else if (isEmpty(h2)) {
      val m = meld(h1, h2)
      val l = findAndDelete(m)

      l == l.sorted
    } else {
      val m = meld(h1, h2)
      val l = findAndDelete(m)

      l == l.sorted
    }
  }
}
