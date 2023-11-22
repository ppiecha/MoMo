package model
import core.Types
import core.Types.{Channel, NoteEvents, Playable, mergeEvents}

case class Track(
    name: Option[String], // todo - required so it can be passed to command
    channel: Int,
    instrument: Int,
    versions: List[TrackVersion],
    active: Option[Boolean]
) extends Playable {
  require((0 until 16) contains channel, s"Channel $channel not in (0, 16) range")
  require((0 until 256) contains instrument, s"Midi value $instrument not in (0, 255) range")

  def getNoteEvents(implicit ppq: Int, channel: Types.Channel = Channel(0)): NoteEvents = {
    val trackEvents = versions.filter(_.active.getOrElse(true)).map(_.getNoteEvents(ppq, Channel(this.channel)))
    mergeEvents(trackEvents)
  }

}
