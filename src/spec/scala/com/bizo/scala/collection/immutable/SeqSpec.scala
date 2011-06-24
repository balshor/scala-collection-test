package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection.TraversableSpec
import com.bizo.scala.collection.GenericCompanionView

object SeqSpec extends TraversableSpec {

  "immutable Seq" should {
    import scala.collection.immutable.Seq
    "using the base implementation" in {
      implicit val _ = Seq
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _ = new GenericCompanionView[Seq](Seq)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
  }  
  
}