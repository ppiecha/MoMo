package model

case class Track(
  name: Option[String],
  channel: Int,
  instrument: Int,
  versions: List[TrackVersion],
  active: Option[Boolean]
) {
  require((0 until 16) contains channel, s"Channel $channel not in (0, 16) range")
  require((0 until 256) contains instrument, s"Midi value $instrument not in (0, 255) range")

  def withDefaults() = this.copy(
    active = if (active.isEmpty) Option(true) else active
  )
}

