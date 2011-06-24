package com.bizo.scala.collection.mutable

import org.specs._
import com.bizo.scala.collection.TraversableSpec
import com.bizo.scala.collection.GenericCompanionView

object ArrayBufferSpec extends TraversableSpec {

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
  }

}