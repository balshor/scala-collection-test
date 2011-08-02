package com.bizo.scala.collection.mutable

import org.specs._
import com.bizo.scala.collection._

object HashSetSpec extends TraversableSpec with SetLikeSpec {

  "mutable HashSets" should {
    import scala.collection.mutable.HashSet
    "using the base implementation" in {
      implicit val _ = HashSet
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy gensetlike examples" in { genSetLikeExamples[HashSet] }
      "satisfy setlike examples" in { setLikeExamples[HashSet] }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[HashSet](HashSet)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

}