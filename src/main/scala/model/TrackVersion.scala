package model

case class TrackVersion(
  name: String,
  notesOn: String,
  timing: String,
  start: Long,
  duration: String,
  velocity: Option[String],
  active: Boolean = true
)
