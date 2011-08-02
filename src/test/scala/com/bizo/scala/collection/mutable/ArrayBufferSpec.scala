package com.bizo.scala.collection.mutable

import org.specs._
import com.bizo.scala.collection._

object ArrayBufferSpec extends TraversableSpec {

  override val isOrdered = true
  
  "mutable ArrayBuffers" should {
    import scala.collection.mutable.ArrayBuffer
    "using the base implementation" in {
      implicit val _ = ArrayBuffer
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[ArrayBuffer](ArrayBuffer)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[ArrayBuffer](ArrayBuffer)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }

}