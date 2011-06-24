package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection.TraversableSpec
import com.bizo.scala.collection.GenericCompanionView

object ListSpec extends TraversableSpec {

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
}