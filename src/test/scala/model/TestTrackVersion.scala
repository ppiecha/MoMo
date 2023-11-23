package model

import core.Types._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues._
import javax.sound.midi.ShortMessage._

class TestTrackVersion extends AnyFlatSpec with Matchers {

  "getTiming" should "calculate timings incrementally" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "seq(Seq(4, 8, 8, 16))",
      start = Option(0),
      duration = ""
    )
    val iterator = trackVersion.getTiming(4)
    iterator.success.value.toStream should contain inOrder (4, 6, 8, 9)
  }

  "getTiming" should "move all values equally based on start" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "seq(Seq(4, 0.5, 8, 16))",
      start = Option(4),
      duration = ""
    )
    val iterator = trackVersion.getTiming(4)
    iterator.success.value.toStream should contain inOrder (8, 40, 42, 43)
  }

  "getMidiNote" should "return iterator of Midi values" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "seq(Seq(64, 66, 68))",
      timing = "",
      start = Option(0),
      duration = ""
    )
    val iterator = trackVersion.getNote
    iterator.success.value.toStream should contain inOrder (MidiValue(64), MidiValue(66), MidiValue(68))
  }

  "getMidiNote" should "return chord as a sequence" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "seq(Seq(Seq(64, 65), 66, 68))",
      timing = "",
      start = Option(0),
      duration = ""
    )
    val iterator = trackVersion.getNote
    iterator.success.value.toStream should contain inOrder
      (Chord(Seq(MidiValue(64), MidiValue(65))), MidiValue(66), MidiValue(68))
  }

  "getDuration" should "return iterator with duration converted to tick/long" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "",
      start = Option(0),
      duration = "seq(Seq(4, 8, 0.5))"
    )
    val iterator = trackVersion.getDuration(2)
    iterator.success.value.toStream should contain inOrder (2, 1, 16)
  }

  "getVelocity" should "return default velocity if it's not defined" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "",
      start = Option(0),
      duration = "seq(Seq(4, 8, 0.5))"
    )
    val iterator = trackVersion.getVelocity
    iterator.success.value.toStream should not be empty
  }

  "getVelocity" should "return Midi values" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "",
      timing = "",
      start = Option(0),
      duration = "",
      velocity = Some("seq(Seq(100), -1)")
    )
    val iterator = trackVersion.getVelocity
    iterator.success.value.toStream should contain (MidiValue(100))
  }

  "getEvents" should "return iterator of NoteOn and NoteOff events" in {
    val trackVersion = TrackVersion(
      name = None,
      notesOn = "seq(Seq(4, 4, 4, 4))",
      timing = "seq(Seq(4, 4, 4, 4))",
      start = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))"
    )
    val iterator = trackVersion.getNoteEvents(2, Channel(0))
    iterator.success.value.toStream should contain inOrder
      (
        NoteEvent(NoteMessage(MidiValue(NOTE_ON), Channel(0), MidiValue(4), MidiValue(100)), 2),
        NoteEvent(NoteMessage(MidiValue(NOTE_ON), Channel(0), MidiValue(4), MidiValue(100)), 4)
      )
  }



}
