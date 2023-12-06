package console

import cats.effect._
import cats.effect.std.Console
import console.AppState._
import core.Exception.ArgError
import core.{FileIO, Yaml}
import model.Composition
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import types._

import javax.sound.midi.Sequence

case class AppState(args: Args, file: File, playback: Playback)

object AppState {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  // type CF = Option[CompositionFile]
  type Args = List[String]
  type File = Option[String]
  type Playback = Option[FiberIO[Unit]]

  def getCmdOptions(args: Args): IO[CmdOptions] = {
    implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    IO(new CmdOptions(args)).handleErrorWith(e => logger.error(e.getMessage).map(_ => new CmdOptions(Nil)))
  }

  def getFile(cmd: CmdOptions, state: AppState, isFileRequired: Boolean): IO[File] = cmd.composition.toOption match {
    case None =>
      if (isFileRequired) IO.raiseError(ArgError("Path to composition file is required"))
      else IO(state.file)
    case file => IO(file)
  }

  private def stop(playback: Playback): IO[Unit] = playback match {
    case Some(playback) =>
      for {
        _ <- playback.cancel
        _ <- playback.join
        _ <- logger.info("playback cancelled")
      } yield ()
    case None => IO.unit >> logger.info("nothing to cancel")
  }

  private def readComposition(file: File): IO[Composition] = file match {
    case Some(fileName) =>
      for {
        yaml <- FileIO.asString(fileName)
        composition <- Yaml.mapYaml(yaml)
      } yield composition
    case None => IO.raiseError(ArgError("Composition file not defined"))
  }

  private def parseComposition(composition: Composition, playOptions: PlayOptions): IO[Sequence] =
    IO(MidiSequence.fromNoteEvents(composition.getNoteEvents(playOptions))(playOptions))

  private def playSequence(sequence: Sequence, playOptions: PlayOptions): IO[Playback] =
    for (fib <- Player.play(sequence, playOptions).start) yield Some(fib)

  private def play(state: AppState): IO[Playback] =
    for {
      _ <- stop(state.playback)
      composition <- readComposition(state.file)
      sequence <- parseComposition(composition, composition.playOptions())
      playback <- playSequence(sequence, composition.playOptions())
    } yield playback

  def getPlayback(cmd: CmdOptions, state: AppState): IO[Playback] = cmd.subcommand match {
    case Some(cmd.play) => play(state)
    case Some(cmd.stop) => stop(state.playback) >> IO.pure(None)
    case _              => logger.info("command ignored") >> IO.pure(None)
    // case cmd => throw ArgError(s"Unknown subcommand ${cmd.toString}")
  }

  def exit(command: String): Either[ExitCode, Args] = {
    val end = command.toLowerCase == "q"
    val args = if (command.isBlank) Nil else command.split(' ').toList
    if (end) Left(ExitCode.Success) else Right(args)
  }

  def processState(state: IO[AppState], isFileRequired: Boolean): IO[ExitCode] =
    for {
      state <- state
      cmd <- getCmdOptions(state.args)
      file <- getFile(cmd, state, isFileRequired)
      playback <- getPlayback(cmd, state)
      _ <- Console[IO].print("> ")
      command <- Console[IO].readLine
      code <- exit(command) match {
        case Left(code)  => IO.pure(code)
        case Right(args) => processState(IO.pure(AppState(args, file, playback)), file.isEmpty)
      }
    } yield code

}
