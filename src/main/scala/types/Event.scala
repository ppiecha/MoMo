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

sealed trait Message extends Channel {
  validateChannel()
}
case class NoteMessage(command: MidiValue, channel: Int, note: MidiValue, velocity: MidiValue) extends Message
case class ProgramMessage(channel: Int, bank: MidiValue, program: MidiValue) extends Message
case class ControlMessage(channel: Int, control: MidiValue, value: MidiValue) extends Message
