package console

import java.io.File
import scopt.{OParser, OParserBuilder}

sealed trait Action
case object Play extends Action
case object Stop extends Action
case object Exit extends Action
case object DoNothing extends Action

case class Config(yamlFile: Option[File] = None, action: Action = DoNothing)

object CmdOptions {

  val builder: OParserBuilder[Config] = OParser.builder[Config]
  val parser: OParser[Unit, Config] = {
    import builder._
    def validateFile(f: File): Either[String, Unit] =
      if (f.exists()) success else failure(s"File $f doesn't exist")
    OParser.sequence(
      programName("MoMo"),
      head("MoMo", "version 0.1.0 Copyright 2023 Piotr Piecha"),
      opt[File]('y', "yaml")
        .valueName("<file>")
        .validate(validateFile)
        .action((f, c) => c.copy(yamlFile = Some(f)))
        .text("YAML file with composition"),
//      opt[File]('f', "font")
//        .valueName("<file>")
//        .validate(validateFile)
//        .action((f, c) => c.copy(soundFile = Some(f)))
//        .text("file with soundfont"),
      opt[Unit]('p', "play")
        .action((_, c) => c.copy(action = Play))
        .text("plays composition"),
      opt[Unit]('s', "stop")
        .action((_, c) => c.copy(action = Stop))
        .text("stops playback"),
      help("help")
      //.action((_, c) => c.copy(action = Help))
        .text("prints this usage text"),
      version("version")
      //.action((_, c) => c.copy(action = Version))
        .text("prints application version"),
      opt[Unit]('e', "exit")
        .action((_, c) => c.copy(action = Exit))
        .text("terminates application"),
      checkConfig(
        config =>
          if (false) failure("message")
          else success)
    )
  }
}
//import org.rogach.scallop._
//import org.rogach.scallop.exceptions.{Help, Version}
//
//import java.io.File

//class CmdOptions(args: Seq[String]) extends ScallopConf(args) {
//  banner(
//    """
//      |MIDI sequencer generating and playing compositions stored in YAML files
//      |
//      |Example: java -jar MoMo.jar -c composition.yaml
//      |
//      |For usage see below:
//      |""".stripMargin
//  )
//
//  version("MoMo MIDI generator and sequencer version 1.0.0 Copyright 2023 Piotr Piecha")
//
//  val yaml: ScallopOption[File] = opt[File]("yaml", descr = "YAML file with composition")
//  validateFileExists(yaml)
//  val font: ScallopOption[File] = opt[File]("font", descr = "File with soundfont")
//  validateFileExists(font)
//  //.opt[List[Double]]("params") // default converters are provided for all primitives and for lists of primitives
//  val play: ScallopOption[Boolean] = opt[Boolean]("play", descr = "Plays composition")
//  val stop: ScallopOption[Boolean] = opt[Boolean]("stop", descr = "Stops playback")
//  val exit: ScallopOption[Boolean] = opt[Boolean]("exit", descr = "Exits application")
//  val version: ScallopOption[Boolean] = opt[Boolean]("version", noshort = true, descr = "Prints version")
//  val help: ScallopOption[Boolean] = opt[Boolean]("help", descr = "Shows this message")
//
//  mutuallyExclusive(play, stop, version, help)
//
//  verify()
//
//  override def onError(e: Throwable): Unit = e match {
//    case Help("") => AppState.log("onError")
//    case Version => AppState.log(getVersionString().getOrElse("No version info defined"))
//    case other => throw other
//  }
//
//}
