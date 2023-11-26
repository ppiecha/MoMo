package console

import types._

import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import scala.util.{Failure, Success, Try}

object MidiSequence {

  def midiMessage(command: MidiValue, channel: Int, data1: MidiValue, data2: MidiValue): ShortMessage = {
    val msg = new ShortMessage()
    //println("midiMessage", channel, command, data1, data2)
    msg.setMessage(command.value, channel, data1.value, data2.value)
    msg
  }

  def makeMidiMessages(message: Message): Seq[ShortMessage] =
    message match {
      case NoteMessage(command, channel, note, velocity) =>
        Seq(midiMessage(command, channel, note, velocity))
      case ProgramMessage(channel, bank, program) =>
        Seq(
          midiMessage(MidiValue(CONTROL_CHANGE), channel, 0, bank.value >> 7),
          midiMessage(MidiValue(CONTROL_CHANGE), channel, 32, bank.value & 0x7f),
          midiMessage(MidiValue(PROGRAM_CHANGE), channel, program, 0)
        )
      case ControlMessage(channel, control, value) =>
        Seq(midiMessage(CONTROL_CHANGE, channel, control, value))
    }

  def makeMidiEvents(event: Event): Seq[MidiEvent] = {
    makeMidiMessages(event.message).map(msg => new MidiEvent(msg, event.tick))
  }

  def fromNoteEvents(noteEvents: Events)(implicit opt: PlayOptions): Try[Sequence] = Try {
    val sequence = new Sequence(Sequence.PPQ, opt.PPQ, 1)
    noteEvents match {
      case Failure(exception) => throw exception
      case Success(iter) =>
        iter
          .flatMap(makeMidiEvents)
          .foreach(sequence.getTracks.head.add)
        sequence
    }
  }

}
