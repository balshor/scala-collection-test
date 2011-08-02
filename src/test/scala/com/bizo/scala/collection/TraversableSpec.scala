package com.bizo.scala.collection

import java.util.{ ArrayList => JArrayList, Arrays => JArrays, Collections => JCollections }
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.{ GenTraversable, TraversableLike, TraversableViewLike, Seq }
import scala.collection.generic.GenericCompanion
import scala.collection.mutable.Builder

import com.bizo.matchers._

import org.specs._

// disable default implicits
import Predef.{ _ => _ }

trait TraversableSpec extends Specification {
  val isOrdered = false

  def matchElements[B](elements: Array[B]) = {
    if (isOrdered) {
      matchElementsInOrder(elements)
    } else {
      matchElementsInAnyOrder(elements)
    }
  }

  def genTraversableLikeExamples[A[X] <: scala.collection.Traversable[X]](implicit factory: GenericCompanion[A]) = {

    val t1234Array = Array(1, 2, 3, 4)
    def t1234: Traversable[Int] = {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      builder += 3
      builder += 4

      builder.result
    }

    def empty: Traversable[Int] = {
      factory.newBuilder[Int].result
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

      (first.result ++ second.result) must matchElements(Array(1, 2, 3, 4, 5, 6, 7, 8))
    }

    "collect" in {
      val partialFunction = new PartialFunction[Int, Int] {
        def isDefinedAt(n: Int) = n % 2 == 0
        def apply(n: Int) = 2 * n
      }

      (t1234.collect(partialFunction)) must matchElements(Array(4, 8))
      (empty.collect(partialFunction)) must beEmpty
    }

    "count" in {
      (t1234.count { _ % 2 == 0 }) mustEqual 2
      (empty.count { _ % 2 == 0 }) mustEqual 0
    }

    "drop" in {
      t1234.drop(2) must matchSize(2)
      empty.drop(2) must beEmpty
    }

    "exists" in {
      (t1234 exists { _ % 2 == 0 }) mustEqual true
      (t1234 exists { _ % 5 == 0 }) mustEqual false
      (empty exists { _ % 2 == 0 }) mustEqual false
    }

    "filter" in {
      (t1234 filter { _ % 2 == 0 }) must matchElements(Array(2, 4))
      (empty filter { _ % 2 == 0 }) must beEmpty
    }

    "filterNot" in {
      (t1234 filterNot { _ % 2 == 0 }) must matchElements(Array(1, 3))
      (empty filterNot { _ % 2 == 0 }) must beEmpty
    }

    "find" in {
      (t1234 find { _ % 3 == 0 }) mustEqual Some(3)
      (t1234 find { _ % 5 == 0 }) mustEqual None
      (empty find { _ % 3 == 0 }) mustEqual None
    }

    "flatMap" in {
      (t1234 flatMap { n => Seq(n, n + 10) }) must matchElements(Array(1, 11, 2, 12, 3, 13, 4, 14))
      (empty flatMap { n => Seq(n, n + 10) }) must beEmpty
    }

    "fold" in {
      t1234.fold(100)(_ + _) mustEqual 110
      empty.fold(100)(_ + _) mustEqual 100
    }

    "foldLeft" in {
      t1234.foldLeft(100)(_ + _) mustEqual 110
      empty.foldLeft(100)(_ + _) mustEqual 100
    }

    "foldRight" in {
      t1234.foldRight(100)(_ + _) mustEqual 110
      empty.foldRight(100)(_ + _) mustEqual 100
    }

    "forall" in {
      val acc = new ConcurrentLinkedQueue[Int]
      t1234 foreach { n => acc.add(n) }

      acc.contains(1) mustEqual true
      acc.contains(2) mustEqual true
      acc.contains(3) mustEqual true
      acc.contains(4) mustEqual true
      acc.size mustEqual 4

      acc.clear()
      empty foreach { n => acc.add(n) }
      acc.isEmpty mustEqual true
    }

    "groupBy" in {
      val grouped = t1234 groupBy { _ % 2 }

      (grouped.size) mustEqual 2
      grouped(0) must matchElements(Array(2, 4))
      grouped(1) must matchElements(Array(1, 3))

      empty groupBy { _ % 2 } must beEmpty
    }

    "hasDefiniteSize" in {
      t1234.hasDefiniteSize // must just not throw an exception
      empty.hasDefiniteSize mustEqual true
    }

    "head" in {
      val entry = t1234 head

      entry must beElementOf(t1234Array)

      try {
        empty head

        fail
      } catch {
        case e: NoSuchElementException => // expected, pass silently
      }
    }

    "headOption" in {
      val entryOption = t1234 headOption

      entryOption.isDefined mustEqual true

      val entry = entryOption.get

      entry must beElementOf(t1234Array)

      empty.headOption.isDefined mustEqual false
    }

    "init" in {
      t1234.init must matchSize(3)

      try {
        empty.init
      } catch {
        case e: UnsupportedOperationException => // expected, pass silently
      }
    }

    "inits" in {
      val collection = t1234

      // Views are not supported.  See, eg https://issues.scala-lang.org/browse/SI-3117
      if (!collection.isInstanceOf[TraversableViewLike[_, _, _]]) {
        val inits = t1234 inits
        var size = 4
        for (elem <- inits) {
          elem must matchSize(size)
          size -= 1
          elem must beSubsetOf(t1234Array)
        }
      }
    }

    "isEmpty" in {
      t1234.isEmpty mustEqual false
      empty.isEmpty mustEqual true
    }

    "last" in {
      val last = t1234.last

      last must beElementOf(t1234Array)
      try {
        empty last

        fail
      } catch {
        case e: NoSuchElementException => // expected, pass silently
      }
    }

    "lastOption" in {
      val lastOption = t1234.lastOption

      lastOption.isDefined mustEqual true
      lastOption.get must beElementOf(t1234Array)

      empty.lastOption.isDefined mustEqual false
    }

    "map" in {
      val mapped = t1234 map { n => n + 10 }
      mapped must matchElements(Array(11, 12, 13, 14))

      empty map { n => n + 10 } must beEmpty
    }

    "partition" in {
      val (even, odd) = t1234 partition { _ % 2 == 0 }
      even must matchElements(Array(2, 4))
      odd must matchElements(Array(1, 3))

      {
        val (nonPositive, positive) = t1234 partition { _ <= 0 }
        positive must matchElements(t1234Array)
        nonPositive must beEmpty
      }

      {
        val (positive, nonPositive) = t1234 partition { _ > 0 }
        positive must matchElements(t1234Array)
        nonPositive must beEmpty
      }

      {
        val (left, right) = empty partition { _ > 0 }
        left must beEmpty
        right must beEmpty
      }
    }

    "repr" in {
      t1234.repr must matchElements(t1234Array)
      empty.repr must beEmpty
    }

    "scan" in {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      val scanned = builder.result.scan(0)(_ + _)

      scanned must matchSize(3)

      // in unordered collections, the result is also unordered
      if (scanned.isInstanceOf[Seq[Int]]) {
        scanned.head mustEqual 0
        scanned.tail.head must beElementOf(Array(1, 2))
        scanned.last mustEqual 3
      }
    }

    "scanLeft" in {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      val scanned = builder.result.scanLeft(0)(_ + _)

      scanned must matchSize(3)

      // in unordered collections, the result is also unordered
      if (scanned.isInstanceOf[Seq[Int]]) {
        scanned.head mustEqual 0
        scanned.tail.head must beElementOf(Array(1, 2))
        scanned.last mustEqual 3
      }
    }

    // Sometimes fails due to result ordering in 2.9.0-1.  See https://issues.scala-lang.org/browse/SI-4161
    "scanRight" in {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      val scanned = builder.result.scanRight(0)(_ + _)

      scanned must matchSize(3)

      // in unordered collections, the result is also unordered
      if (scanned.isInstanceOf[Seq[Int]]) {
        scanned.head mustEqual 3
        scanned.tail.head must beElementOf(Array(1, 2))
        scanned.last mustEqual 0
      }
    }

    "slice" in {
      try {
        val sliced = t1234 slice (1, 3)
        sliced must matchSize(2)
        sliced must beSubsetOf(t1234Array)
      } catch {
        case u: UnsupportedOperationException => {
          fail("slice is not supported")
        }
      }
    }

    "span" in {
      val (taken, dropped) = t1234 span (_ < 2)

      for (element <- taken) {
        (element < 2) mustEqual true
      }

      (taken ++ dropped) must matchElements(t1234Array)
    }

    "splitAt" in {
      val (prefix, suffix) = t1234 splitAt 1

      prefix must matchSize(1)
      suffix must matchSize(3)

      (prefix ++ suffix) must matchElements(t1234Array)
    }

    "stringPrefix" in {
      val prefix = t1234 stringPrefix

      prefix must not(beEqualTo(null))
      prefix.length must not(beEqualTo(0))
    }

    "tail" in {
      val tail = t1234 tail

      tail must matchSize(3)
      tail must beSubsetOf(t1234Array)
    }

    "tails" in {
      val collection = t1234

      // Views are not supported.  See, eg https://issues.scala-lang.org/browse/SI-3117
      if (!collection.isInstanceOf[TraversableViewLike[_, _, _]]) {
        val tails = collection tails
        var size = 4
        for (elem <- tails) {
          elem must matchSize(size)
          size -= 1
          elem must beSubsetOf(t1234Array)
        }
      }
    }

    "take" in {
      val taken = t1234 take 3

      taken must matchSize(3)
      taken must beSubsetOf(t1234Array)
    }

    "takeWhile" in {
      try {
        val taken = t1234 takeWhile (_ % 2 != 0)

        for (elem <- taken) {
          (elem % 2 != 0) mustEqual true
        }
        taken must beSubsetOf(t1234Array)
      } catch {
        case unsupported: UnsupportedOperationException => {
          fail("takeWhile is not supported")
        }
      }
    }

    "toIterator" in { // Note: do not use matchers in this test as the matchers use iterators.
      val iterator = t1234.toIterator
      val elements = new JArrayList[Integer](4)
      while (iterator.hasNext) {
        elements.add(iterator.next)
      }
      JCollections.sort(elements);

      val expected = new JArrayList[Integer](4)
      expected.add(1)
      expected.add(2)
      expected.add(3)
      expected.add(4)

      elements mustEqual expected
    }

    "toStream" in {
      val iterator = t1234.toStream.toIterator
      val elements = new JArrayList[Integer](4)
      while (iterator.hasNext) {
        elements.add(iterator.next)
      }
      JCollections.sort(elements);

      val expected = new JArrayList[Integer](4)
      expected.add(1)
      expected.add(2)
      expected.add(3)
      expected.add(4)

      elements mustEqual expected
    }

    "toString" in {
      val strRep = t1234.toString
      strRep must not(beEqualTo(null))
      (strRep.length > 0) mustEqual true
    }

    "toTraversable" in {
      val iterator = t1234.toTraversable.toIterator
      val elements = new JArrayList[Integer](4)
      while (iterator.hasNext) {
        elements.add(iterator.next)
      }
      JCollections.sort(elements);

      val expected = new JArrayList[Integer](4)
      expected.add(1)
      expected.add(2)
      expected.add(3)
      expected.add(4)

      elements mustEqual expected
    }

    /* Views are tested by re-running all (compatible) tests using a GenericCompanionView.
    "view(int,int)" in {

    }

    "view" in {

    }
    */

    "withFilter" in {
      val filtered = t1234 withFilter { _ % 2 == 0 }

      val elements = new JArrayList[Integer](2)
      for (elem <- filtered) {
        elements.add(elem)
      }
      JCollections.sort(elements);

      val expected = new JArrayList[Integer](2)
      expected.add(2)
      expected.add(4)

      elements mustEqual expected
    }
  }

  def traversableExamples[A[X] <: Traversable[X]](implicit factory: GenericCompanion[A]) = {
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

      (outerBuilder.result.flatten) must matchElements(Array(1, 2, 3, 4, 5, 6))
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

class GenericCompanionRangeView[A[X] <: Traversable[X]](companion: GenericCompanion[A], min: Int = 0, max: Int = 10) extends GenericCompanion[Traversable] {
  private def newViewBuilder[X](builder: Builder[X, A[X]]) = new Builder[X, Traversable[X]] {
    def +=(elem: X) = { builder += elem; this }
    def clear() { builder.clear() }
    def result() = { builder.result.view(min, max) }
  }

  def newBuilder[X]: Builder[X, Traversable[X]] = {
    newViewBuilder(companion.newBuilder[X])
  }
}

sealed trait OrderedProperty
case class IsOrdered extends OrderedProperty
case class NotOrdered extends OrderedProperty