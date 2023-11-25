package types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import types._

class TestScale extends AnyFlatSpec with Matchers {

  val scale = Scale(Seq(2, 4, 5, 7, 9, 11).map(Positive[Int](_)), MidiValue(60), Positive(12))

  "Scale.toMidiValue" should "return rootMidiNote for CMaj root 60 and step 1" in {
    scale.toMidiValue(0) shouldBe scale.rootMidiNote
  }

  "Scale.toMidiValue" should "return D (62) for CMaj root 60 and step 1" in {
    scale.toMidiValue(1) shouldBe MidiValue(62)
  }

  "Scale.toMidiValue" should "return B (59) for CMaj root 60 and step -1" in {
    scale.toMidiValue(-1) shouldBe MidiValue(59)
  }
}
