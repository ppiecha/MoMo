package core

import org.scalatest.flatspec.AnyFlatSpec
import core.Sequence._

class TestSequence extends AnyFlatSpec {

  "seq" should "repeat sequence infinitely" in {
    val s = seq(Seq(1, 2, 3))
    assert(s.take(7).toSeq == Seq(1, 2, 3, 1, 2, 3, 1))
  }

  "seq" should "repeat sequence given number of times" in {
    val s = seq(Seq(1, 2, 3), repeat = Option(2))
    assert(s.take(7).toSeq == Seq(1, 2, 3, 1, 2, 3))
  }

}
