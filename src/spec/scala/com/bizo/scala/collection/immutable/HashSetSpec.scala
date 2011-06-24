package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection.TraversableSpec
import com.bizo.scala.collection.GenericCompanionView

object HashSetSpec extends TraversableSpec {

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

}