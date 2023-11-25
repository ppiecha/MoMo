package console

import types._

import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import scala.util.{Failure, Success, Try}

object MidiSequence {

  def midiMessage(command: MidiValue, channel: Channel, data1: MidiValue, data2: MidiValue): ShortMessage = {
    val msg = new ShortMessage()
    msg.setMessage(command.value, channel.number, data1.value, data2.value)
    msg
  }

  def makeMidiMessages(message: Message): Seq[ShortMessage] =
    message match {
      case NoteMessage(command, channel, note, velocity) =>
        Seq(midiMessage(command, channel, note, velocity))
      case ProgramMessage(channel, bank, program) =>
        Seq(
          midiMessage(MidiValue(CONTROL_CHANGE), channel, MidiValue(0), MidiValue(bank.value >> 7)),
          midiMessage(MidiValue(CONTROL_CHANGE), channel, MidiValue(32), MidiValue(bank.value & 0x7f)),
          midiMessage(MidiValue(PROGRAM_CHANGE), channel, program, MidiValue(0))
        )
      case ControlMessage(channel, control, value) =>
        Seq(midiMessage(MidiValue(CONTROL_CHANGE), channel, control, value))
    }

  def makeMidiEvents(event: Event): Seq[MidiEvent] =
    makeMidiMessages(event.message).map(msg => new MidiEvent(msg, event.tick))

  def fromNoteEvents(noteEvents: Events)(implicit ppq: Int): Try[Sequence] = Try {
    val sequence = new Sequence(Sequence.PPQ, ppq, 1)
    noteEvents match {
      case Failure(exception) => throw exception
      case Success(iter) =>
        iter.flatMap(makeMidiEvents).foreach(sequence.getTracks.head.add)
        sequence
    }
  }

}
