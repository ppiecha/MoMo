package core

import scala.io.Source
import scala.util.Try

// TODO Support for reading compositions from disk

object IO {
  def readFile(fileName: String) = {
    val source = Source.fromFile(fileName)
    Try(try source.mkString finally source.close())
  }

}
