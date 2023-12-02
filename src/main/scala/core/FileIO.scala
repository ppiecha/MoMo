package core

import scala.io._
import cats.effect._

object FileIO {

  def source(fileName: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(IO(Source.fromFile(fileName)))

  def asString(fileName: String): IO[String] =
    source(fileName).use(s => IO.blocking(s.mkString))

}
