import com.typesafe.scalalogging.Logger
import core.{IO, Yaml}
import model.Composition

import scala.util.{Failure, Success, Try}

object App extends App {

  private val logger = Logger(getClass.getName)
  private def getCompositionName(a: Array[String]) = Try(a(0))

  val composition = for {
    fileName <- getCompositionName(args)
    yaml <- IO.readFile(fileName)
    composition <- Yaml.mapYaml(yaml)
  } yield composition
  composition match {
    case Failure(exception) => logger.error(exception.toString); throw exception
    case Success(value) => logger.info(composition.toString)
  }
  //open file
  //map to yaml composition case class
  //load
  //play
}
