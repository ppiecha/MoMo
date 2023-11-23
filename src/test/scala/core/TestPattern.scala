package core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import core.Pattern._

class TestPattern extends AnyFlatSpec with Matchers {

  "seq" should "return original Seq for default values" in {
    val s = seq(Seq(1, 2, 3))
    s.take(7).toSeq shouldBe Seq(1, 2, 3)
  }

  "seg" should "bla" in {
    val s = seq(Seq(Seq(1, 4), 2, 3))
    s.take(7).toSeq shouldBe Seq(Seq(1, 4), 2, 3)
  }

  "seq" should "repeat sequence infinitely when repeat is set to -1" in {
    val s = seq(Seq(1, 2, 3), repeat = -1)
    s.take(7).toSeq shouldBe Seq(1, 2, 3, 1, 2, 3, 1)
  }

  "seq" should "repeat sequence given number of times" in {
    val s = seq(Seq(1, 2, 3), repeat = 2)
    s.take(7).toSeq shouldBe Seq(1, 2, 3, 1, 2, 3)
  }

  "seq" should "respect offset" in {
    val s = seq(Seq(1, 2, 3), repeat = 2, offset = 1)
    s.take(7).toSeq shouldBe Seq(2, 3, 1, 2, 3, 1)
  }

  "seq" should "return nothing if repeat is zero" in {
    val s = seq(Seq(1, 2, 3), repeat = 0)
    s.take(1).toSeq shouldBe Seq()
  }

  "seq" should "take offset from the end for negative offset value" in {
    val s = seq(Seq(1, 2, 3), offset = -1)
    s.take(2).toSeq shouldBe Seq(3, 1)
  }

  "seq" should "be combined using ++ operator" in {
    val s = seq(Seq(1, 2, 3)) ++ seq(Seq(3, 2, 1))
    s.take(6). toSeq shouldBe Seq(1, 2, 3, 3, 2, 1)
  }

  "repeat" should "be method on iterator which repeats given operator given number of times" in {
    val s = seq(Seq(1, 2, 3)).repeat(2)
    s.take(7).toSeq shouldBe Seq(1, 2, 3, 1, 2, 3)
  }

  "seq" should "return empty iterator for empty sequence" in {
    seq(Seq()) shouldBe empty
  }

}
