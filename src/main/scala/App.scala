import com.typesafe.scalalogging.Logger
import console.Player
import core.{IO, Yaml}

import scala.util.{Failure, Success, Try}

object App extends App {

  private val logger = Logger(getClass.getName)

  private def getCompositionName(a: Array[String]) = Try(a(0))

  private val status = for {
    fileName <- getCompositionName(args)
    yaml <- IO.readFile(fileName)
    composition <- Yaml.mapYaml(yaml)
    _ <- Player.reloadSoundBank(composition.soundFontPath)(Player.synth)
    sequence <- Player.play(composition, composition.BPM)(composition.resolution)
  } yield sequence

  status match {
    case Failure(exception) => throw exception
    case Success(_)         => ()
  }

  var command = ""
  while (command.toLowerCase != "q") {
    print("> ")
    command = scala.io.StdIn.readLine()
  }
  Player.close()
  System.exit(0)
}
