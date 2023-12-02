import cats.effect._
import model.AppState
import cats.effect.std.Console
import console.Command.processCommand
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object App extends IOApp {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private def loop(state: IO[AppState]): IO[ExitCode] =
    for {
      state <- processCommand(state)
      _ <- Console[IO].print("> ")
      command <- Console[IO].readLine
      args = if (command.isBlank) Nil else command.split(' ').toList
      code <- if (command.toLowerCase != "q") {
        loop(IO(state.copy(args = args)))
      } else
        IO(ExitCode.Success)
    } yield code

  override def run(args: List[String]): IO[ExitCode] = loop(IO.pure(AppState(args)))

}
