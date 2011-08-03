package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection._

object ListSpec extends TraversableSpec with IterableViewSpec {

  override val isOrdered = true
  
  "immutable Lists" should {
    import scala.collection.immutable.List
    "using the base implementation" in {
      implicit val _ = List
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _1 = new GenericCompanionView[List](List)
      implicit val _2 = new CompanionIterableView[List](List)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[List](List)
      implicit val _2 = new CompanionIterableRangeView[List](List)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
  }  
}