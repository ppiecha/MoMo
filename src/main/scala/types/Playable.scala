package types

case class PlayOptions(
    soundFile: SoundFile = None,
    PPQ: Int = DEFAULT_PPQ,
    BPM: Int = DEFAULT_BPM,
    LENGTH_LIMIT: Double = DEFAULT_LENGTH_LIMIT
)

trait Playable {
  def getNoteEvents(implicit opt: PlayOptions, channel: Int = 0): Events[Event]
}
