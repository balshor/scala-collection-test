package com.bizo.scala.collection

import java.util.{ ArrayList => JArrayList, Arrays => JArrays }
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.{ GenTraversable, TraversableLike }
import scala.collection.generic.GenericCompanion
import scala.collection.mutable.Builder

import org.specs._
import org.specs.matcher.Matcher

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner

import Predef.{ _ => _ }

@RunWith(classOf[JUnitSuiteRunner])
object TraversableSpec extends Specification {

  "immutable Seq" should {
    import scala.collection.immutable.Seq
    "using the base implementation" in {
      implicit val _ = Seq
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[Seq](Seq)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

  "immutable Lists" should {
    import scala.collection.immutable.List
    "using the base implementation" in {
      implicit val _ = List
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[List](List)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

  "mutable ArrayBuffers" should {
    import scala.collection.mutable.ArrayBuffer
    "using the base implementation" in {
      implicit val _ = ArrayBuffer
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[ArrayBuffer](ArrayBuffer)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

  "immutable HashSets" should {
    import scala.collection.immutable.HashSet
    "using the base implementation" in {
      implicit val _ = HashSet
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

  "mutable HashSets" should {
    import scala.collection.mutable.HashSet
    "using the base implementation" in {
      implicit val _ = HashSet
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[scala.collection.mutable.HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

  def genTraversableLikeExamples[A[X] <: scala.collection.Traversable[X]](implicit factory: GenericCompanion[A]) = {
    def t1234: Traversable[Int] = {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      builder += 3
      builder += 4

      builder.result
    }

    "concatenate" in {
      val first = factory.newBuilder[Int]
      first += 1
      first += 2
      first += 3

      val second = factory.newBuilder[Int]
      second += 4
      second += 5
      second += 6
      second += 7
      second += 8

      (first.result ++ second.result) must matchElementsInAnyOrder(Array(1, 2, 3, 4, 5, 6, 7, 8))
    }

    "collect" in {
      val partialFunction = new PartialFunction[Int, Int] {
        def isDefinedAt(n: Int) = n % 2 == 0
        def apply(n: Int) = 2 * n
      }

      (t1234.collect(partialFunction)) must matchElementsInAnyOrder(Array(4, 8))
    }

    "count" in {
      (t1234.count { _ % 2 == 0 }) mustEqual 2
    }

    "drop" in {
      (t1234.drop(2).size) mustEqual 2
    }

    "exists" in {
      (t1234 exists { _ % 2 == 0 }) mustEqual true
      (t1234 exists { _ % 5 == 0 }) mustEqual false
    }

    "filter" in {
      (t1234 filter { _ % 2 == 0 }) must matchElementsInAnyOrder(Array(2, 4))
    }

    "filterNot" in {
      (t1234 filterNot { _ % 2 == 0 }) must matchElementsInAnyOrder(Array(1, 3))
    }

    "find" in {
      (t1234 find { _ % 3 == 0 }) mustEqual Some(3)
      (t1234 find { _ % 5 == 0 }) mustEqual None
    }

    "flatMap" in {
      (t1234 flatMap { n => Seq(n, n + 10) }) must matchElementsInAnyOrder(Array(1, 2, 3, 4, 11, 12, 13, 14))
    }

    "fold" in {
      t1234.fold(100)(_ + _) mustEqual 110
    }

    "foldLeft" in {
      t1234.foldLeft(100)(_ + _) mustEqual 110
    }

    "foldRight" in {
      t1234.foldRight(100)(_ + _) mustEqual 110
    }

    "forall" in {
      val acc = new ConcurrentLinkedQueue[Int]
      t1234 foreach { n => acc.add(n) }

      acc.contains(1) mustEqual true
      acc.contains(2) mustEqual true
      acc.contains(3) mustEqual true
      acc.contains(4) mustEqual true
      acc.size mustEqual 4
    }

    "groupBy" in {
      val grouped = t1234 groupBy { _ % 2 }

      (grouped.size) mustEqual 2
      grouped(0) must matchElementsInAnyOrder(Array(2, 4))
      grouped(1) must matchElementsInAnyOrder(Array(1, 3))
    }
  }

  def traversableExamples[A[X] <: Traversable[X]](implicit factory: GenericCompanion[A]) = {
    "satisfy equality when elements are equal" in {
      val first = factory.newBuilder[Int]
      first += 1
      first += 2

      val second = factory.newBuilder[Int]
      second += 1
      second += 2

      first.result must_== second.result
    }

    "flatten nested traversables" in {
      val innerBuilder1 = factory.newBuilder[Int]
      innerBuilder1 += 1
      innerBuilder1 += 2

      val innerBuilder2 = factory.newBuilder[Int]
      innerBuilder2 += 3
      innerBuilder2 += 4

      val innerBuilder3 = factory.newBuilder[Int]
      innerBuilder3 += 5
      innerBuilder3 += 6

      val outerBuilder = factory.newBuilder[A[Int]]
      outerBuilder += innerBuilder1.result
      outerBuilder += innerBuilder2.result
      outerBuilder += innerBuilder3.result

      (outerBuilder.result.flatten) must matchElementsInAnyOrder(Array(1, 2, 3, 4, 5, 6))
    }

    "transpose nested traversables" in {
      val innerBuilder1 = factory.newBuilder[Int]
      innerBuilder1 += 1
      innerBuilder1 += 2

      val innerBuilder2 = factory.newBuilder[Int]
      innerBuilder2 += 3
      innerBuilder2 += 4

      val innerBuilder3 = factory.newBuilder[Int]
      innerBuilder3 += 5
      innerBuilder3 += 6

      val outerBuilder = factory.newBuilder[A[Int]]
      outerBuilder += innerBuilder1.result
      outerBuilder += innerBuilder2.result
      outerBuilder += innerBuilder3.result

      (outerBuilder.result.transpose) must matchNestedElementsInAnyOrder(Array(Array(1, 3, 5), Array(2, 4, 6)))
    }
  }
}

sealed trait MatchAnyOrder[A] {
  def matches(left: Array[A], right: GenTraversable[A]): Boolean = {
    if (left.length != right.size) {
      return false
    }
    val leftCopy = new JArrayList[A](left.length)
    for (i <- (0 until left.length)) {
      leftCopy.add(left(i))
    }
    for (elem <- right) {
      val found = leftCopy.remove(elem)
      if (!found) {
        return false
      }
    }
    return leftCopy.isEmpty
  }

  def matchesNested(left: Array[Array[A]], right: GenTraversable[GenTraversable[A]]): Boolean = {
    val leftCopy = new JArrayList[Array[A]](left.length)
    for (i <- (0 until left.length)) {
      leftCopy.add(left(i))
    }
    for (subTraversable <- right) {
      var found = false
      for (i <- (0 until leftCopy.size)) {
        if (!found && matches(leftCopy.get(i), subTraversable)) {
          leftCopy.remove(i)
          found = true
        }
      }
      if (!found) {
        return false
      }
    }
    leftCopy isEmpty
  }
}

case class matchElementsInAnyOrder[A](expected: Array[A]) extends Matcher[GenTraversable[A]] with MatchAnyOrder[A] {
  def apply(actual: => GenTraversable[A]): (Boolean, String, String) =
    (matches(expected, actual),
      "GenTraversable contains expected elements.",
      "GenTraversable does not match expected elements.  Found [%s], expected [%s]".format(actual, JArrays.asList(expected)))
}

case class matchNestedElementsInAnyOrder[A](expected: Array[Array[A]]) extends Matcher[GenTraversable[GenTraversable[A]]] with MatchAnyOrder[A] {
  def apply(actual: => GenTraversable[GenTraversable[A]]): (Boolean, String, String) = {
    (matchesNested(expected, actual), "GenTraversable contains expected nested elements", "GenTraversable does not match expected elements.")
  }

}

case class matchElementsInOrder[A](expected: Array[A]) extends Matcher[GenTraversable[A]] {
  def apply(actual: => GenTraversable[A]): (Boolean, String, String) = {
    val it = actual.toIterator
    for (i <- (0 until expected.length)) {
      if (!it.hasNext) {
        return (false, "(ignored)", "insufficient elements in input at index " + i)
      }
      val next = it.next
      if (expected(i) != next) {
        return (false, "(ignored)", "non-equal elements at index %d: expected %s, actual %s".format(i, expected(i), next))
      }
    }
    (true, "input contains expected elements in order", "(ignored)")
  }
}

class GenericCompanionView[A[X] <: Traversable[X]](companion: GenericCompanion[A]) extends GenericCompanion[Traversable] {
  private def newViewBuilder[X](builder: Builder[X, A[X]]) = new Builder[X, Traversable[X]] {
    def +=(elem: X) = { builder += elem; this }
    def clear() { builder.clear() }
    def result() = { builder.result.view }
  }

  def newBuilder[X]: Builder[X, Traversable[X]] = {
    newViewBuilder(companion.newBuilder[X])
  }
}
