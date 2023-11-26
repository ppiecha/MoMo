package types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TestScale extends AnyFlatSpec with Matchers {

  val scale: Scale = Scale("2 4 5 7 9 11", 60)

  "Scale.toMidiValue" should "return rootMidiNote for CMaj root 60 and step 1" in {
    scale.toMidiValue(0) shouldBe MidiValue(scale.rootMidiNote)
  }

  "Scale.toMidiValue" should "return D (62) for CMaj root 60 and step 1" in {
    scale.toMidiValue(1) shouldBe MidiValue(62)
  }

  "Scale.toMidiValue" should "return B (59) for CMaj root 60 and step -1" in {
    scale.toMidiValue(-1) shouldBe MidiValue(59)
  }

  "Scale.toMidiValue" should "return 72 for CMaj root 60 and step 7" in {
    scale.toMidiValue(7) shouldBe MidiValue(72)
  }
}
