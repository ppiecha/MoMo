package model

import core.Types.{Channel, MidiValue, NoteEvent, NoteOff, NoteOn}
import org.scalatest.flatspec.AnyFlatSpec

class TestTrackVersion extends AnyFlatSpec {

  "getTiming" should "calculate timings incrementally" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "seq(Seq(4, 8, 8, 16))",
      start = Option(0),
      duration = ""
    )
    val res0 = trackVersion.getTiming(4).get
    assert(res0.take(4).toSeq == Seq(4, 6, 8, 9))
    val res1 = trackVersion.getTiming(4).get.next()
  }

  "getTiming" should "move all values equally based on start" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "seq(Seq(4, 0.5, 8, 16))",
      start = Option(4),
      duration = ""
    )
    val res0 = trackVersion.getTiming(4).get
    assert(res0.take(6).toSeq == Seq(8, 40, 42, 43))
  }

  "getMidiNote" should "return iterator of Midi values" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "seq(Seq(64, 66, 68))",
      timing = "",
      start = Option(0),
      duration = ""
    )
    val res0 = trackVersion.getMidiNote().get
    assert(res0.take(6).toSeq == Seq(MidiValue(64), MidiValue(66), MidiValue(68)))
  }

  "getDuration" should "return iterator with duration converted to tick/long" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "",
      start = Option(0),
      duration = "seq(Seq(4, 8, 0.5))"
    )
    val res0 = trackVersion.getDuration(2).get
    assert(res0.take(6).toSeq == Seq(2, 1, 16))
  }

  "getVelocity" should "return iterator of Midi values" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "",
      start = Option(0),
      duration = "seq(Seq(4, 8, 0.5))"
    )
    val res0 = trackVersion.getVelocity().get
    assert(res0.take(2).toSeq == Seq(MidiValue(100), MidiValue(100)))
  }

  "getEvents" should "return iterator of NoteOn and NoteOff events" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "seq(Seq(4, 4, 4, 4))",
      timing = "seq(Seq(4, 4, 4, 4))",
      start = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))"
    )
    val res0 = trackVersion.getEvents(2, Channel(0)).get
    assert(res0.take(2).toSeq ==
      Seq(
        NoteEvent(NoteOn(Channel(0), MidiValue(4), MidiValue(100)), 2),
        NoteEvent(NoteOff(Channel(0), MidiValue(4), MidiValue(100)), 4)
      ))
  }

}
