package model

import cats.effect.{ExitCode, FiberIO, IO}
import console.CmdOptions
import model.AppState.CF
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import types._

import javax.sound.midi.Sequence

case class CompositionFile(name: String)

sealed trait AppState {

  def cf: CF

  def toExitCode: ExitCode = this match {
    case Exiting(_) => ExitCode.Success
    case _          => ExitCode.Error
  }
}
case class StartingUp(args: List[String], cf: CF) extends AppState
case class Listening(player: Option[FiberIO[Unit]], cf: CF) extends AppState
case class ReadingArgs(args: List[String], player: Option[FiberIO[Unit]], cf: CF) extends AppState
case class ReadingComposition(cf: CF) extends AppState
case class ParsingComposition(composition: Composition, cf: CF) extends AppState
case class StartingPlayback(sequence: Sequence, playOptions: PlayOptions, cf: CF) extends AppState
case class Playing(player: FiberIO[Unit], cf: CF) extends AppState
case class Exiting(cf: CF) extends AppState

object AppState {

  type CF = Option[CompositionFile]

  def getCmdOptions(args: List[String]): IO[CmdOptions] = {
    implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    IO(new CmdOptions(args)).handleErrorWith(e => logger.error(e.getMessage).map(_ => new CmdOptions(Nil)))
  }

}
