package com.bizo.scala.collection.immutable

import scala.collection.generic.GenericCompanion
import scala.collection.mutable.Builder
import org.specs._
import com.bizo.scala.collection._

object HashSetSpec extends TraversableSpec with SetLikeSpec with IterableViewSpec {

  "immutable HashSets" should {
    import scala.collection.immutable.HashSet
    "using the base implementation" in {
      implicit val _ = HashSet
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy gensetlike examples" in { genSetLikeExamples[HashSet] }
      "satisfy setlike examples" in { setLikeExamples[HashSet] }
    }
    "using views" in {
      implicit val _1 = new GenericCompanionView[HashSet](HashSet)
      implicit val _2 = new CompanionIterableView[HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
    "using range views" in {
      implicit val _1 = new GenericCompanionRangeView[HashSet](HashSet)
      implicit val _2 = new CompanionIterableRangeView[HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
  }

}
