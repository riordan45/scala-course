package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genEmptyHeap: Gen[H] = const(empty)

  lazy val genNonEmptyHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- genHeap
  } yield insert(x, h)

  lazy val genHeap: Gen[H] = oneOf(genEmptyHeap, genNonEmptyHeap)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertMinAndGetMin") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insertTwoAndGetMin") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("deleteMinOfSingleElement") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("deleteMinMaintainsOrder") = forAll { (h: H) =>
    def getAllElements(h: H): List[Int] =
      if isEmpty(h) then Nil
      else findMin(h) :: getAllElements(deleteMin(h))

    val sortedElements = getAllElements(h)
    sortedElements == sortedElements.sorted
  }

  // Additional properties to catch issues with Bogus3 and Bogus4

  property("meldedHeapContainsAllElements") = forAll { (h1: H, h2: H) =>
    def getAllElements(h: H): List[Int] =
      if isEmpty(h) then Nil
      else findMin(h) :: getAllElements(deleteMin(h))

    val elements1 = getAllElements(h1)
    val elements2 = getAllElements(h2)
    val meldedHeap = meld(h1, h2)
    val meldedElements = getAllElements(meldedHeap)

    meldedElements.toSet == (elements1 ++ elements2).toSet
  }

  property("deleteMinYieldsSortedElements") = forAll { (h: H) =>
    def getAllElements(h: H): List[Int] =
      if isEmpty(h) then Nil
      else findMin(h) :: getAllElements(deleteMin(h))

    val elements = getAllElements(h)
    elements == elements.sorted
  }

  property("insertingAndDeletingAllElementsShouldResultInAnEmptyHeap") = forAll { (elements: List[Int]) =>
    val heap = elements.foldLeft(empty)((h, e) => insert(e, h))
    val emptyHeap = elements.foldLeft(heap)((h, _) => deleteMin(h))
    isEmpty(emptyHeap)
  }

  property("findMinAfterMeldingTwoHeaps") = forAll { (h1: H, h2: H) =>
    val min1 = if isEmpty(h1) then Int.MaxValue else findMin(h1)
    val min2 = if isEmpty(h2) then Int.MaxValue else findMin(h2)
    val meldedHeap = meld(h1, h2)
    val meldedMin = if isEmpty(meldedHeap) then Int.MaxValue else findMin(meldedHeap)
    meldedMin == math.min(min1, min2)
  }
}
