package console

import cats.effect._
import cats.effect.std.Console
import console.AppState._
import core.Exception.ArgError
import core.{FileIO, Yaml}
import model.Composition
import types._
import scopt.{OEffect, OParser}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.sound.midi.Sequence

case class AppState(yamlFile: YamlFile, soundFile: SoundFile, playback: Playback)

object AppState {

  private type Args = List[String]
  private type YamlFile = Option[java.io.File]
  private type SoundFile = Option[java.io.File]
  private type Playback = Option[FiberIO[Unit]]
  private type Options = (Option[Config], List[OEffect])

  private def printEffects(effects: List[OEffect]): IO[Unit] = effects match {
    case ::(head, next) =>
      val e = head match {
        case OEffect.DisplayToOut(msg)  => Console[IO].println(msg)
        case OEffect.DisplayToErr(msg)  => Console[IO].println(msg)
        case OEffect.ReportError(msg)   => Console[IO].println("Error: " + msg)
        case OEffect.ReportWarning(msg) => Console[IO].println("Warning: " + msg)
        case OEffect.Terminate(_)       => IO.unit
      }
      e >> printEffects(next)
    case Nil => IO.unit
  }

  def log(message: String, includeTime: Boolean = true): IO[Unit] = {
    val time = DateTimeFormatter.ofPattern("HH:mm:ss:SSS").format(LocalDateTime.now) + " "
    val msg = s"${if (includeTime) time else ""}$message"
    Console[IO].println(msg)
  }

  def handler[A](default: A): Throwable => IO[A] = { e: Throwable =>
    log(e.getMessage).map(_ => default)
  }

  private def getInput(state: AppState): IO[String] = {
    val fileName = state.yamlFile match {
      case Some(path) => " [" + path.getName + "] "
      case None       => ""
    }
    val prefix = s"MoMo$fileName> "
    Console[IO].print(prefix) >> Console[IO].readLine
  }

  private def getYamlFile(options: Options, state: AppState): IO[YamlFile] = options match {
    case (_, effects) if effects.nonEmpty => IO.pure(None)
    case (config, _)                      => IO.pure(config.flatMap(c => c.yamlFile).orElse(state.yamlFile))
  }

  private def stop(playback: Playback): IO[Unit] = playback match {
    case Some(playback) =>
      for {
        _ <- playback.cancel
        _ <- playback.join
      } yield ()
    case None => IO.unit
  }

  private def readComposition(file: YamlFile): IO[Composition] = file match {
    case Some(fileName) =>
      for {
        yaml <- FileIO.asString(fileName.getPath)
        composition <- Yaml.mapYaml(yaml)
      } yield composition
    case None => IO.raiseError(ArgError("Composition file not defined"))
  }

  private def parseComposition(composition: Composition, playOptions: PlayOptions): IO[Sequence] =
    IO(MidiSequence.fromNoteEvents(composition.getNoteEvents(playOptions))(playOptions))

  private def playSequence(sequence: Sequence, playOptions: PlayOptions): IO[Playback] = //for {
    for (fib <- Player.play(sequence, playOptions).start) yield Some(fib)
    //_ <- log(s"all loaded, playing ${Option(pair)}")
    //playback <- Player.waitWhilePlaying(pair._2).start.map(Option(_))
  //} yield playback

  private def play(state: AppState, file: YamlFile): IO[Playback] =
    for {
      _ <- stop(state.playback)
      composition <- readComposition(file)
      _ <- log(s"before parse")
      sequence <- parseComposition(composition, composition.playOptions())
      _ <- log("after parse")
      playback <- playSequence(sequence, composition.playOptions())
    } yield playback

  private def getPlayback(options: Options, state: AppState, file: YamlFile): IO[Playback] = options match {
    case (_, effects) if effects.nonEmpty => IO.pure(None)
    case (Some(config), _) =>
      config.action match {
        case Play => play(state, file)
        case Stop => stop(state.playback) >> IO.pure(None)
        case _    => IO.pure(None)
      }
    case _ => IO.pure(None)
  }

  private def exit(command: String): Either[ExitCode, Args] = {
    val args = if (command.isBlank) Nil else command.split(' ').toList
    OParser.runParser(CmdOptions.parser, args, Config()) match {
      case (Some(config), _) if config.action == Exit => Left(ExitCode.Success)
      case _                                          => Right(args)
    }
  }

  def processState(args: Args, state: AppState): IO[ExitCode] =
    for {
      options <- IO.pure(OParser.runParser(CmdOptions.parser, args, Config()))
      (_, effects) = options
      _ <- printEffects(effects)
      yamlFile <- getYamlFile(options, state)
      playback <- getPlayback(options, state, yamlFile).handleErrorWith(handler(None))
      state = AppState(yamlFile, None, playback)
      command <- getInput(state)
      code <- exit(command) match {
        case Left(code)  => IO.pure(code)
        case Right(args) => processState(args, state)
      }
    } yield code

}
