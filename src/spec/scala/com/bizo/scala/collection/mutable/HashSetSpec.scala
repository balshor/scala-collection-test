package com.bizo.scala.collection.mutable

import org.specs._
import com.bizo.scala.collection.TraversableSpec
import com.bizo.scala.collection.GenericCompanionView

object HashSetSpec extends TraversableSpec {

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

}