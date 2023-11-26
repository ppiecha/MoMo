package model

import types._

case class Track(
    name: String,
    channel: Int,
    bank: Int,
    instrument: Int,
    versions: List[TrackVersion],
    active: Option[Boolean]
) extends Playable
    with Channel {
  require((0 until 256) contains instrument, s"Instrument midi value $instrument not in (0, 255) range")

  def getNoteEvents(implicit opt: PlayOptions, channel: Int = 0): Events = {
    val trackEvents = versions.filter(_.active.getOrElse(true)).map(_.getNoteEvents(opt, this.channel))
    val programEvent = ProgramEvent(ProgramMessage(channel, MidiValue(bank), MidiValue(instrument)), 0)
    val programEvents = Events.fromSeqOfEvents(Seq(programEvent))
    Events.mergeEvents(programEvents +: trackEvents)
  }

}
