package model

import types._

case class Track(
    name: String,
    channel: Int,
    bank: Int,
    instrument: Int,
    versions: List[TrackVersion],
    active: Option[Boolean]
) extends Playable {
  require((0 until 16) contains channel, s"Channel $channel not in (0, 16) range")
  require((0 until 256) contains instrument, s"Midi value $instrument not in (0, 255) range")

  def getNoteEvents(implicit ppq: Int, channel: Channel = Channel(0)): Events = {
    val trackEvents = versions.filter(_.active.getOrElse(true)).map(_.getNoteEvents(ppq, Channel(this.channel)))
    val programEvent = ProgramEvent(ProgramMessage(channel, MidiValue(bank), MidiValue(instrument)), 0)
    val programEvents = Events.fromSeqOfEvents(Seq(programEvent))
    Events.mergeEvents(programEvents +: trackEvents)
  }

}
