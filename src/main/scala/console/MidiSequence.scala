package console

import core.Types.{Channel, NoteEvent, NoteEvents}

import javax.sound.midi.{MidiEvent, Sequence, ShortMessage}
import scala.util.{Failure, Success, Try}

object MidiSequence {

  def makeEvent(command: Int, channel: Int, note: Int, velocity: Int, tick: Int) = {
    val msg = new ShortMessage()
    msg.setMessage(command, channel, note, velocity)
    new MidiEvent(msg, tick)
  }

  def toMidiEvent(noteEvent: NoteEvent): MidiEvent =
    makeEvent(
      noteEvent.noteMessage.command.value,
      noteEvent.noteMessage.channel.number,
      noteEvent.noteMessage.note.value,
      noteEvent.noteMessage.velocity.value,
      noteEvent.tick.toInt
    )

  def fromNoteEvents(noteEvents: NoteEvents)(implicit ppq: Int): Try[Sequence] = Try {
    val sequence = new Sequence(Sequence.PPQ, ppq, 1)
    noteEvents match {
      case Failure(exception) => throw exception
      case Success(iter) =>
        iter.map(toMidiEvent).foreach(sequence.getTracks.head.add)
        sequence
    }
  }

}
