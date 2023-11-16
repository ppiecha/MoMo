import com.typesafe.scalalogging.Logger
import core.Types.Channel
import core.{IO, Yaml}

import scala.util.{Failure, Success, Try}

object App extends App {

  private val logger = Logger(getClass.getName)
  private def getCompositionName(a: Array[String]) = Try(a(0))

  private val events = for {
    fileName <- getCompositionName(args)
    yaml <- IO.readFile(fileName)
    composition <- Yaml.mapYaml(yaml)
    events <- composition.tracks.head.versions.head.getEvents(448, Channel(0))
  } yield events
  events match {
    case Failure(exception) => throw exception
    case Success(value) =>
      val head = value.next()
      logger.info(value.take(5).toSeq.toString())
  }

}
