package types

trait Playable {
  def getNoteEvents(implicit ppq: Int, channel: Channel = Channel(0)): Events
}
