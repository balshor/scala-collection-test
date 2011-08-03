package com.bizo.scala.collection.immutable

import org.specs._
import com.bizo.scala.collection._

object SeqSpec extends TraversableSpec with IterableViewSpec {

  override val isOrdered = true
  
  "immutable Seq" should {
    import scala.collection.immutable.Seq
    "using the base implementation" in {
      implicit val _ = Seq
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
    }
    "using views" in {
      implicit val _1 = new GenericCompanionView[Seq](Seq)
      implicit val _2 = new CompanionIterableView[Seq](Seq)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
    "using range views" in {
      implicit val _ = new GenericCompanionRangeView[Seq](Seq)
      implicit val _2 = new CompanionIterableRangeView[Seq](Seq)
      "satisfy gentraversablelike examples" in { genTraversableLikeExamples }
      "satisfy traversable examples" in { traversableExamples }
      "satisfy iterableviewlike examples" in { iterableViewLikeExamples }
    }
  }  
  
}