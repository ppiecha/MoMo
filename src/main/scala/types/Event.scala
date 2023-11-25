package types

/**
  * MIDI events and messages
  */
sealed trait Event {
  def message: Message
  def tick: Long
}
case class NoteEvent(message: NoteMessage, tick: Long) extends Event
case class ProgramEvent(message: ProgramMessage, tick: Long) extends Event
case class ControlEvent(message: ControlMessage, tick: Long) extends Event

sealed trait Message
case class NoteMessage(command: MidiValue, channel: Channel, note: MidiValue, velocity: MidiValue) extends Message
case class ProgramMessage(channel: Channel, bank: MidiValue, program: MidiValue) extends Message
case class ControlMessage(channel: Channel, control: MidiValue, value: MidiValue) extends Message
