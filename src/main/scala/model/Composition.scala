package model

import core.Types
import core.Types.{Channel, NoteEvents, Playable, mergeEvents}

import scala.util.Try

case class Composition(
    name: Option[String],
    BPM: Int,
    soundFontPath: Option[String],
    tracks: List[Track],
    author: Option[String],
    description: Option[String],
    comment: Option[String],
    var PPQ: Option[Int]
) extends Playable {
  require(BPM >= 0, s"BPM $BPM should not be negative")
  require(PPQ.getOrElse(0) >= 0, s"PPQ $PPQ should not be negative")

  def resolution: Int = if (PPQ.isEmpty) 480 else PPQ.get

  def getNoteEvents(implicit ppq: Int, channel: Types.Channel = Channel(0)): NoteEvents = {
    val compositionEvents = tracks.filter(_.active.getOrElse(true)).map(t => t.getNoteEvents)
    mergeEvents(compositionEvents)
  }

  // todo - validate uniqueness of track and version names
  def validate(): Try[Boolean] = ???

}
