package com.bizo.scala.collection.mutable

import org.specs._
import com.bizo.scala.collection._

object ArrayBufferSpec extends TraversableSpec with IterableViewSpec {

  override val isOrdered = true
  
  "mutable ArrayBuffers" should {
    import scala.collection.mutable.ArrayBuffer
    "using the base implementation" in {
      implicit val _ = ArrayBuffer
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _1 = new GenericCompanionView[ArrayBuffer](ArrayBuffer)
      implicit val _2 = new CompanionIterableView[ArrayBuffer](ArrayBuffer)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[ArrayBuffer](ArrayBuffer)
      implicit val _2 = new CompanionIterableRangeView[ArrayBuffer](ArrayBuffer)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
  }

}