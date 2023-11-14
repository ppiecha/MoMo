package model

import core.Types.{Channel, MidiValue}

case class Track(
  name: String,
  channel: Int, //Channel,
  instrument: Int, //MidiValue,
  versions: List[TrackVersion],
  active: Boolean = true
)
