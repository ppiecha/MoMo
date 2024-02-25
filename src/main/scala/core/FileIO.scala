package core

import scala.io._
import cats.effect._

object FileIO {

  private def source(fileName: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable {
      for {
        file <- IO(Source.fromFile(fileName))
        //_ <- logger.info("source")
      } yield file
    }

  def asString(fileName: String): IO[String] =
    source(fileName)
      .use { s =>
        for {
          lines <- IO(s.mkString)
          //_ <- logger.info("asString"+"\n"+lines)
        } yield lines
      }
}
