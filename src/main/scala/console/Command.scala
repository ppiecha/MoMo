package console

import core._
import model._
import model.AppState._
import cats.effect._
import cats.effect.std.Console
import core.Exception.ArgError
import types._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import javax.sound.midi.Sequence

object Command {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private def listening(player: Option[FiberIO[Unit]], cf: CF): IO[AppState] =
    for {
      _ <- Console[IO].print("> ")
      command <- Console[IO].readLine
      state = command match {
        case command if command.toLowerCase != "q" =>
          ReadingArgs(if (command.isBlank) Nil else command.split(' ').toList, player, cf)
        case _ => Exiting(cf)
      }
    } yield state

  private def readingArgs(args: List[String], player: Option[FiberIO[Unit]], cf: CF): IO[AppState] =
    getCmdOptions(args) map { options =>
      options.composition.toOption match {
        case Some(fileName) => ReadingComposition(Some(CompositionFile(fileName)))
        case None =>
          options.subcommand match {
            case Some(options.play) => stop(player); ReadingComposition(cf)
            case Some(options.stop) => stop(player); Listening(None, cf)
            case cmd                => throw ArgError(s"Unknown subcommand ${cmd.toString}")
          }
        case options => throw ArgError(s"Unknown command $options")
      }
    }

  private def readingComposition(cf: CF): IO[AppState] = cf match {
    case Some(compositionFile) =>
      for {
        yaml <- FileIO.asString(compositionFile.name)
        composition <- Yaml.mapYaml(yaml)
      } yield ParsingComposition(composition, cf)
    case None => IO.raiseError(ArgError("Composition file not defined"))
  }

  private def parsingComposition(composition: Composition, cf: CF): IO[AppState] = {
    implicit val playOptions: PlayOptions =
      PlayOptions(composition.resolution, composition.BPM, composition.lengthLimit)
    IO(StartingPlayback(MidiSequence.fromNoteEvents(composition.getNoteEvents), playOptions, cf))
  }

  private def startingPlayback(sequence: Sequence, playOptions: PlayOptions, cf: CF): IO[AppState] =
    Player.play(sequence, playOptions).start.map(Playing(_, cf))

  private def stop(player: Option[FiberIO[Unit]]): IO[Unit] = player match {
    case Some(p) => IO(p.cancel >> p.join)
    case None    => IO.unit
  }

  private def cancelling(player: FiberIO[Unit], cf: CF): IO[AppState] =
    (player.cancel >> player.join).map(_ => Listening(None, cf))

  private def playing(player: FiberIO[Unit], cf: CF): IO[AppState] =
    IO(Listening(Some(player), cf))

  def processState(state: IO[AppState], cf: CF): IO[AppState] = state flatMap {
    case StartingUp(args, cf)                        => processState(readingArgs(args, None, cf), cf)
    case Listening(player, cf)                       => processState(listening(player, cf), cf)
    case ReadingArgs(args, player, cf)               => processState(readingArgs(args, player, cf), cf)
    case ReadingComposition(compositionFile)         => processState(readingComposition(compositionFile), cf)
    case ParsingComposition(composition, cf)         => processState(parsingComposition(composition, cf), cf)
    case StartingPlayback(sequence, playOptions, cf) => processState(startingPlayback(sequence, playOptions, cf), cf)
    case Playing(player, cf)                         => processState(playing(player, cf), cf)
    case Exiting(cf)                                 => IO.pure(Exiting(cf))
  }

}
