package model

import core.Types.Positive

case class Composition (
  name: String,
  BPM: Positive[Int],
  soundFontPath: String,
  tracks: Seq[Track],
  author: Option[String] = None,
  description: Option[String] = None,
  comment: Option[String] = None,
  PPQ: Positive[Int] = Positive(480)
)
