package model

import cats.effect.IO
import console.CmdOptions
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class AppState(args: List[String], fileName: Option[String] = None, composition: Option[Composition] = None) {
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  def getCmdOptions: IO[CmdOptions] =
    IO(new CmdOptions(args)).handleErrorWith(e => logger.error(e.getMessage).map(_ => new CmdOptions(Nil)))
}
