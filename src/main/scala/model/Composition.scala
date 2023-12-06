package model

import types._

import scala.util.Try

case class Composition(
    name: Option[String],
    BPM: Int,
    lengthLimitInSeconds: Option[Double],
    midiFilePath: Option[String],
    soundFontPath: Option[String],
    tracks: List[Track],
    startAt: Option[Double], //todo
    author: Option[String],
    description: Option[String],
    comment: Option[String],
    var PPQ: Option[Int]
) extends Playable {
  require(BPM >= 0, s"BPM $BPM should not be negative")
  require(PPQ.getOrElse(0) >= 0, s"PPQ $PPQ should not be negative")

  val resolution: Int = if (PPQ.isEmpty) DEFAULT_PPQ else PPQ.get

  val lengthLimit: Double = if (lengthLimitInSeconds.isEmpty) DEFAULT_LENGTH_LIMIT else lengthLimitInSeconds.get

  val offset: Double = if (startAt.isEmpty) 0.0 else startAt.get

  def getNoteEvents(implicit opt: PlayOptions, channel: Int = 0): Events[Event] = {
    val compositionEvents = tracks.filter(_.active.getOrElse(true)).map(t => t.getNoteEvents)
    Events.mergeEvents(compositionEvents)
  }

  def playOptions(): PlayOptions = PlayOptions(resolution, BPM, lengthLimit)

  // todo - validate uniqueness of track and version names
  def validate(): Try[Boolean] = ???

}
