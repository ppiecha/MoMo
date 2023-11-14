package model

//import com.fasterxml.jackson.annotation.JsonProperty
import core.Types.Positive

import scala.beans.BeanProperty

case class Composition (
  name: String,
  BPM: Int, //Positive[Int],
  soundFontPath: String,
  tracks: List[Track],
  author: Option[String] = None,
  description: Option[String] = None,
  comment: Option[String] = None,
  PPQ: Int = 480 //Positive[Int] = Positive(480)
)
