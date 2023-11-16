package model

case class Composition (
  name: Option[String],
  BPM: Int,
  soundFontPath: String,
  tracks: List[Track],
  author: Option[String],
  description: Option[String],
  comment: Option[String],
  var PPQ: Option[Int]
) {
  require(BPM >= 0, s"BPM $BPM should not be negative")
  require(PPQ.getOrElse(0) >= 0, s"PPQ $PPQ should not be negative")

  def withDefaults() = this.copy(
    PPQ = if (PPQ.isEmpty) Option(480) else PPQ
  )
}

object Composition{
  def apply(
             name: Option[String],
             BPM: Int,
             soundFontPath: String,
             tracks: List[Track],
             author: Option[String] = None,
             description: Option[String] = None,
             comment: Option[String] = None,
             PPQ: Option[Int] = Option(480)
           ) = new Composition(name, BPM, soundFontPath, tracks, author, description, comment, PPQ)
}
