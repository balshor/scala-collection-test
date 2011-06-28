/*
 * These matchers are collection-test friendly in the sense that they rely only on Java collections, Arrays, Ranges, and
 * Scala iterators.
 */
package com.bizo.matchers

import java.util.{ ArrayList => JArrayList }
import scala.collection.GenTraversable
import org.specs.matcher.Matcher

sealed trait MatchAnyOrder[A] {
  def arrayListCopy[A](source: Array[A]): JArrayList[A] = {
    val copy = new JArrayList[A](source.length)
    for (i <- 0 until source.length) {
      copy.add(source(i))
    }
    copy
  }

  def arrayListCopy[A](source: GenTraversable[A]): JArrayList[A] = {
    val copy = new JArrayList[A](source.size) // if size is bugged, we may be inefficient but still correct
    val iterator = source.toIterator
    while (iterator.hasNext) {
      copy.add(iterator.next)
    }
    copy
  }

  def matches(left: Array[A], right: GenTraversable[A]): Boolean = {
    if (left.length != right.size) {
      return false
    }
    val leftCopy = arrayListCopy(left)
    for (elem <- right) {
      val found = leftCopy.remove(elem)
      if (!found) {
        return false
      }
    }
    return leftCopy.isEmpty
  }

  def matchesNested(left: Array[Array[A]], right: GenTraversable[GenTraversable[A]]): Boolean = {
    val leftCopy = arrayListCopy(left)
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
      "GenTraversable does not match expected elements.  Found %s, expected %s".format(actual mkString ("[", ",", "]"), expected mkString ("[", ",", "]")))
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

case class matchSize[A](expected: Int) extends Matcher[GenTraversable[A]] {
  def apply(actual: => GenTraversable[A]): (Boolean, String, String) = {
    var count = 0
    val it = actual.toIterator
    while (it.hasNext) {
      count += 1
      it.next
    }
    (count == expected, "Size matches", "Found %d elements, expected %s.".format(count, expected))
  }
}

case class beEmpty[A] extends matchSize[A](0)

case class beElementOf[A](original: Array[A]) extends Matcher[A] {
  def apply(actual: => A): (Boolean, String, String) = {
    for (i <- 0 until original.length) {
      if (actual == original(i)) {
        return (true, "Actual [%s] is an element of the original collection.".format(actual), "ignored")
      }
    }
    (false, "ignored", "Actual [%s] is not an element of the original collection.".format(actual))
  }
}

case class beSubsetOf[A](original: Array[A]) extends Matcher[GenTraversable[A]] with MatchAnyOrder[A] {
  def apply(actual: => GenTraversable[A]): (Boolean, String, String) = {
    val copy = arrayListCopy(original)

    val iterator = actual.toIterator
    while (iterator.hasNext) {
      val element = iterator.next
      val removed = copy.remove(element)
      if (!removed) {
        return (false, "ignored", "Missing element " + element)
      }
    }
    (true, "All elements found.", "ignored")
  }
}
