package core

import scala.io.Source
import scala.util.Using

object IO {
  def readFile(fileName: String) =
    Using(Source.fromFile(fileName)) { source =>
      source.mkString
    }
}
