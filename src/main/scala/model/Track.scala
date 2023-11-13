package model

import core.Types.{Channel, MidiValue}

case class Track(
  name: String,
  channel: Channel,
  instrument: MidiValue,
  versions: Seq[TrackVersion],
  active: Boolean = true
)
