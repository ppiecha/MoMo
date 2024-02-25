import cats.effect._
import console.AppState
import console.AppState.processState

object App extends IOApp {

  override protected def blockedThreadDetectionEnabled = true

  override def run(args: List[String]): IO[ExitCode] =
    processState(args, AppState(None, None, None))
}
