package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues._
import types._

import javax.sound.midi.ShortMessage._

class TestTrackVersion extends AnyFlatSpec with Matchers {

  implicit val opt: PlayOptions = PlayOptions()

  "getTiming" should "calculate timings incrementally" in {
    val trackVersion =
      TrackVersion(
        name = "test",
        midiNote = None,
        timing = "seq(Seq(4, 8, 8, 16))",
        startAt = Option(0),
        duration = "seq(Seq(4, 4, 4, 4))",
        scale = None,
        scaleNote = None
      )
    val iterator = trackVersion.getTiming(4)
    iterator.success.value.toStream should contain inOrder (4, 6, 8, 9)
  }

  "getTiming" should "move all values equally based on start" in {
    val trackVersion =
      TrackVersion(
        name = "test",
        midiNote = None,
        timing = "seq(Seq(4, 0.5, 8, 16))",
        startAt = Option(4),
        duration = "seq(Seq(4, 4, 4, 4))",
        scale = None,
        scaleNote = None
      )
    val iterator = trackVersion.getTiming(4)
    iterator.success.value.toStream should contain inOrder (8, 40, 42, 43)
  }

  "getNote" should "return iterator of Midi values when midiNote is defined" in {
    val trackVersion =
      TrackVersion(
        name = "test",
        midiNote = Option("seq(Seq(64, 66, 68))"),
        timing = "seq(Seq(4, 0.5, 8, 16))",
        startAt = Option(0),
        duration = "seq(Seq(4, 4, 4, 4))",
        scale = None,
        scaleNote = None
      )
    val iterator = trackVersion.getNote
    iterator.success.value.toStream should contain inOrder
      (SingleValue(MidiValue(64)), SingleValue(MidiValue(66)), SingleValue(MidiValue(68)))
  }

  "getNote" should "return chord as a sequence when midiNote is defined" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = Option("seq(Seq(Seq(64, 65), 66, 68))"),
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = None,
      scaleNote = None
    )
    val iterator = trackVersion.getNote
    iterator.success.value.toStream should contain inOrder
      (Chord(Seq(MidiValue(64), MidiValue(65))), SingleValue(MidiValue(66)), SingleValue(MidiValue(68)))
  }

  "getNote" should "raise exception when neither scaleNote nor midiNote is defined" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = Option(""),
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = None,
      scaleNote = None
    )
    assertThrows[IllegalArgumentException] {
      trackVersion.getNote
    }
  }

  "getNote" should "raise exception when scaleNote and midiNote are defined at the same time" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = Option(""),
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = None,
      scaleNote = Option("")
    )
    assertThrows[IllegalArgumentException] {
      trackVersion.getNote.get.next()
    }
  }

  "getNote" should "return iterator of Midi values when scaleNote is defined" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = None,
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = Option(Scale("2 4 5 7 9 11", 60)),
      scaleNote = Option("seq(Seq(0))")
    )
    val iterator = trackVersion.getNoteEvents
    iterator.success.value.toStream should contain inOrder (
      NoteEvent(NoteMessage(MidiValue(144), 0, MidiValue(60), MidiValue(100)), 480),
      NoteEvent(NoteMessage(MidiValue(128), 0, MidiValue(60), MidiValue(100)), 960)
    )
  }

  "getNote" should "return chord as a sequence when when scaleNote is defined" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = None,
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = Option(Scale("2 4 5 7 9 11", 60)),
      scaleNote = Option("seq(Seq(Seq(-1, 7)))")
    )
    val iterator = trackVersion.getNoteEvents
    iterator.success.value.toStream should contain inOrder (
      NoteEvent(NoteMessage(MidiValue(144), 0, MidiValue(59), MidiValue(100)), 480),
      NoteEvent(NoteMessage(MidiValue(128), 0, MidiValue(59), MidiValue(100)), 960),
      NoteEvent(NoteMessage(MidiValue(144), 0, MidiValue(72), MidiValue(100)), 480),
      NoteEvent(NoteMessage(MidiValue(128), 0, MidiValue(72), MidiValue(100)), 960)
    )
  }

  "getNote" should "raise exception when scaleNote is defined without scale" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = None,
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = None,
      scaleNote = Option("seq(Seq(Seq(-1, 7)))")
    )
    assertThrows[IllegalArgumentException] {
      trackVersion.getNoteEvents.get.next()
    }
  }

  "getDuration" should "return iterator with duration converted to tick/long" in {
    val trackVersion =
      TrackVersion(
        name = "test",
        midiNote = None,
        timing = "seq(Seq(4, 0.5, 8, 16))",
        startAt = Option(0),
        duration = "seq(Seq(4, 8, 0.5))",
        scale = None,
        scaleNote = None
      )
    val iterator = trackVersion.getDuration(2)
    iterator.success.value.toStream should contain inOrder (2, 1, 16)
  }

  "getVelocity" should "return default velocity if it's not defined" in {
    val trackVersion =
      TrackVersion(
        name = "test",
        midiNote = None,
        timing = "seq(Seq(4, 0.5, 8, 16))",
        startAt = Option(0),
        duration = "seq(Seq(4, 8, 0.5))",
        scale = None,
        scaleNote = None
      )
    val iterator = trackVersion.getVelocity
    iterator.success.value.toStream should not be empty
  }

  "getVelocity" should "return Midi values" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = None,
      timing = "seq(Seq(4, 0.5, 8, 16))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      velocity = Some("seq(Seq(100), -1)"),
      scale = None,
      scaleNote = None
    )
    val iterator = trackVersion.getVelocity
    iterator.success.value.toStream should contain(MidiValue(100))
  }

  "getEvents" should "return iterator of NoteOn and NoteOff events" in {
    val trackVersion = TrackVersion(
      name = "test",
      midiNote = Option("seq(Seq(4, 4, 4, 4))"),
      timing = "seq(Seq(4, 4, 4, 4))",
      startAt = Option(0),
      duration = "seq(Seq(4, 4, 4, 4))",
      scale = None,
      scaleNote = None
    )
    val iterator = trackVersion.getNoteEvents(PlayOptions(PPQ = 2))
    iterator.success.value.toStream should contain inOrder
      (
        NoteEvent(NoteMessage(MidiValue(NOTE_ON), 0, MidiValue(4), MidiValue(100)), 2),
        NoteEvent(NoteMessage(MidiValue(NOTE_OFF), 0, MidiValue(4), MidiValue(100)), 4)
      )
  }

}
