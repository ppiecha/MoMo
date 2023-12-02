package console

import core._
import model._
import cats.effect._
import types.PlayOptions
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Command {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private def setFileName(state: IO[AppState]): IO[AppState] =
    for {
      state <- state
      options <- state.getCmdOptions
    } yield state.copy(fileName = options.composition.toOption)

  private def loadCompositionFromFile(state: IO[AppState]): IO[AppState] =
    for {
      state <- setFileName(state)
      state <- (state.fileName, state.composition) match {
        case (None, None)    => IO.raiseError(new IllegalArgumentException("Composition file name not defined"))
        case (None, Some(_)) => IO(state)
        case (Some(fileName), _) =>
          for {
            yaml <- FileIO.asString(fileName)
            composition <- Yaml.mapYaml(yaml)
          } yield state.copy(fileName = Option(fileName), composition = Option(composition))
      }
    } yield state

  def processCommand(state: IO[AppState]): IO[AppState] =
    for {
      state <- Command.loadCompositionFromFile(state)
      composition = state.composition match {
        case Some(composition) => composition
        case None              => throw new RuntimeException("Composition not defined")
      }
      cmd <- state.getCmdOptions
      playOptions = PlayOptions(composition.resolution, composition.BPM, composition.lengthLimit)
      _ <- cmd.subcommand match {
        case None | Some(cmd.play) => Player.play(composition)(playOptions)
        case Some(cmd.stop)        => IO.unit // Player.stop()
      }
      _ <- logger.info("exited")
    } yield state

}
