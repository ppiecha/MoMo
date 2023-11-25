package types

import scala.math.abs

case class Scale(degrees: Seq[Positive[Int]], rootMidiNote: MidiValue, pitchesPerOctave: Positive[Int]) {
  def toMidiValue(step: Int): MidiValue = {
    val octaves = step / pitchesPerOctave.value
    val normStep = step % pitchesPerOctave.value
    val scale = 0 +: degrees.map(_.value) :+ 0
    val transformed =
      if (normStep >= 0) rootMidiNote.value + (octaves * pitchesPerOctave.value) + scale(normStep)
      else rootMidiNote.value + ((octaves - 1) * pitchesPerOctave.value) + scale.reverse(abs(normStep))
    MidiValue(transformed)
  }
}
