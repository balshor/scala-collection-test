package com.bizo.scala.collection

import java.util.{ ArrayList => JArrayList, Arrays => JArrays, Collections => JCollections }
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder

import com.bizo.matchers._

import org.specs._

trait SetLikeSpec extends Specification {

  // Note: GenSetLike is private[collection] and cannot be accessed
  def genSetLikeExamples[A[X] <: SetLike[X, A[X] with Set[X]] with GenTraversable[X]](implicit factory: GenericCompanion[A]) = {
    val s1234Array = Array(1, 2, 3, 4)
    def s1234: SetLike[Int, A[Int] with Set[Int]] = {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      builder += 3
      builder += 4

      builder.result
    }

    def empty: SetLike[Int, A[Int] with Set[Int]] = {
      factory.newBuilder[Int].result
    }

    "&" in {
      s1234 & Set(1, 2, 5, 6) must matchAsSet(Array(1, 2))
    }

    "&~" in {
      s1234 &~ Set(1, 2, 5, 6) must matchAsSet(Array(3, 4))
    }

    "apply" in {
      s1234(1) mustEqual true
      s1234(2) mustEqual true
      s1234(3) mustEqual true
      s1234(4) mustEqual true
      s1234(5) mustEqual false
      s1234(6) mustEqual false
      s1234(7) mustEqual false
      s1234(8) mustEqual false
    }

    "intersect" in {
      s1234 intersect Set(1, 2, 5, 6) must matchAsSet(Array(1, 2))
    }

    "subsetOf" in {
      s1234 subsetOf Set(1, 2, 3, 4) mustEqual true
      s1234 subsetOf Set(1, 2, 3, 4, 5, 6) mustEqual true
      s1234 subsetOf Set(1, 2, 5, 6) mustEqual false
      s1234 subsetOf Set(1, 2) mustEqual false
    }

    "|" in {
      s1234 | Set() must matchAsSet(s1234Array)
      s1234 | Set(1, 2, 3, 4) must matchAsSet(s1234Array)
      s1234 | Set(1, 2, 5, 6) must matchAsSet(Array(1, 2, 3, 4, 5, 6))
      s1234 | Set(5, 6) must matchAsSet(Array(1, 2, 3, 4, 5, 6))
    }

  }

  def setLikeExamples[A[X] <: Set[X]](implicit factory: GenericCompanion[A]) = {
    val s1234Array = Array(1, 2, 3, 4)
    def s1234: Set[Int] = {
      val builder = factory.newBuilder[Int]
      builder += 1
      builder += 2
      builder += 3
      builder += 4

      builder.result
    }

    def empty: Set[Int] = {
      factory.newBuilder[Int].result
    }

    "+" in {
      (s1234 + 5) must matchElementsInAnyOrder(Array(1, 2, 3, 4, 5))
      (s1234 + 4) must matchElementsInAnyOrder(Array(1, 2, 3, 4))
    }

    "-" in {
      (s1234 - 4) must matchElementsInAnyOrder(Array(1, 2, 3))
      (s1234 - 5) must matchElementsInAnyOrder(s1234Array)
      (empty - 1) must beEmpty
    }

    "contains" in {
      s1234.contains(1) mustEqual true
      s1234.contains(2) mustEqual true
      s1234.contains(3) mustEqual true
      s1234.contains(4) mustEqual true
      s1234.contains(5) mustEqual false

      empty.contains(1) mustEqual false
    }

    "empty" in {
      s1234.empty must beEmpty
      empty.empty must beEmpty
    }

    "+ multiple" in {
      (s1234 + (5, 6, 7)) must matchAsSet(Array(1, 2, 3, 4, 5, 6, 7))
      (s1234 + (3, 4, 5, 6)) must matchAsSet(Array(1, 2, 3, 4, 5, 6))
      (s1234 + (3, 3, 4, 4, 5, 6, 6)) must matchAsSet(Array(1, 2, 3, 4, 5, 6))
      (s1234 + (1, 2, 3, 4)) must matchAsSet(s1234Array)
    }

    "++" in {
      (s1234 ++ List()) must matchAsSet(s1234Array)
      (s1234 ++ List(1, 2)) must matchAsSet(s1234Array)
      (s1234 ++ List(1, 2, 5, 6)) must matchAsSet(Array(1, 2, 3, 4, 5, 6))
      (s1234 ++ List(5, 6, 7, 8)) must matchAsSet(Array(1, 2, 3, 4, 5, 6, 7, 8))
    }

    "diff" in {
      (s1234 diff Set(1, 2)) must matchAsSet(Array(3, 4))
      (s1234 diff Set(1, 2, 3, 4)) must beEmpty
      (s1234 diff Set(1, 2, 5, 6)) must matchAsSet(Array(3, 4))
    }

    "isEmpty" in {
      s1234.isEmpty mustEqual false
      empty.isEmpty mustEqual true
    }

    "map" in {
      s1234.map(_ * 2) must matchAsSet(Array(2, 4, 6, 8))
      empty.map(_ * 2) must beEmpty
    }

    "subsets" in {
      s1234.subsets.toArray must matchNestedElementsInAnyOrder(Array(Array[Int](), Array(1), Array(2), Array(3),
        Array(4), Array(1, 2), Array(1, 3), Array(1, 4), Array(2, 3), Array(2, 4), Array(3, 4), Array(2, 3, 4),
        Array(1, 3, 4), Array(1, 2, 4), Array(1, 2, 3), Array(1, 2, 3, 4)))
      empty.subsets.toArray must matchNestedElementsInAnyOrder(Array(Array[Int]()))
    }

    "subsets(Int)" in {
      s1234.subsets(0).toArray must matchNestedElementsInAnyOrder(Array(Array[Int]()))
      s1234.subsets(1).toArray must matchNestedElementsInAnyOrder(Array(Array(1), Array(2), Array(3), Array(4)))
      s1234.subsets(2).toArray must matchNestedElementsInAnyOrder(Array(Array(1, 2), Array(1, 3), Array(1, 4),
        Array(2, 3), Array(2, 4), Array(3, 4)))
      s1234.subsets(3).toArray must matchNestedElementsInAnyOrder(Array(Array(2, 3, 4), Array(1, 3, 4), Array(1, 2, 4), 
        Array(1, 2, 3)))
      s1234.subsets(4).toArray must matchNestedElementsInAnyOrder(Array(Array(1,2,3,4)))
      s1234.subsets(5) must beEmpty
      s1234.subsets(-1) must beEmpty
      
      empty.subsets(0).toArray must matchNestedElementsInAnyOrder(Array(Array[Int]()))
      empty.subsets(1) must beEmpty
      empty.subsets(-1) must beEmpty
    }

    "toArray" in {
      val a1234 = s1234.toArray
      a1234 must matchElementsInAnyOrder(Array(1,2,3,4))
      
      empty.toArray.size mustEqual 0
    }

    "toBuffer" in {
      s1234.toBuffer must matchElementsInAnyOrder(Array(1,2,3,4))
      empty.toBuffer must beEmpty
    }

    "toSeq" in {
      s1234.toSeq must matchElementsInAnyOrder(Array(1,2,3,4))
      empty.toBuffer must beEmpty
    }

    "toString" in {
      s1234.toString.isEmpty mustEqual false
    }

    "union" in {
      val builder = factory.newBuilder[Int]
      builder += 3
      builder += 4
      builder += 5
      builder += 6
      val s3456 = builder.result
      
      s1234 union s3456 must matchAsSet(Array(1,2,3,4,5,6))
      s1234 union empty must matchAsSet(Array(1,2,3,4))
      empty union s1234 must matchAsSet(Array(1,2,3,4))
      empty union empty must beEmpty
    }
  }
}