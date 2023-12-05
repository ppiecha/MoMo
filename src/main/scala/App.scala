import cats.effect._
import model._
import console.Command._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object App extends IOApp {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    processState(IO.pure(StartingUp(args, None)), None).map(_.toExitCode)

}
