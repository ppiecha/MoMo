import com.typesafe.scalalogging.Logger
import core.Types.Channel
import core.{IO, Player, Yaml}

import scala.util.{Failure, Success, Try}

object App extends App {

  private val logger = Logger(getClass.getName)
  private def getCompositionName(a: Array[String]) = Try(a(0))

  private val composition = for {
    fileName <- getCompositionName(args)
    yaml <- IO.readFile(fileName)
    composition <- Yaml.mapYaml(yaml)
  } yield composition
  composition match {
    case Failure(exception) => throw exception
    case Success(value) =>
      //println(value.tracks.head.versions.head.getEvents(480, Channel(0)).get.toSeq.toString)
      val events = Player.playTrackVersion(value.tracks.head.versions.head)(480, Channel(0))
      println(events)
      //logger.info(value.take(5).toSeq.toString())
  }

}
