package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection._

object ListSpec extends TraversableSpec {

  override val isOrdered = true
  
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
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[List](List)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }  
}