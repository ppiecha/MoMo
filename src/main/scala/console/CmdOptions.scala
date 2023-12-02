package console

import org.rogach.scallop._

class CmdOptions(args: Seq[String]) extends ScallopConf(args) {
  banner(
    """
      |MIDI sequencer playing compositions stored in YAML files
      |
      |Example: java -jar MoMo.jar -c composition.yaml
      |
      |For usage see below:
      |""".stripMargin
  )

  val composition: ScallopOption[String] = opt[String]("composition", descr = "YAML file with composition")
  val soundfont: ScallopOption[String] = opt[String]("font", descr = "file with soundfont")

  object play extends Subcommand("play") {
    val track: ScallopOption[String] = opt[String]("track")
    val version: ScallopOption[String] = opt[String]("version")
  }

  object stop extends Subcommand("stop")

  //val capi = toggle("capi", prefix = "no-", descrYes = "enable adding to Windows key-store", descrNo = "disable adding to Windows key-store")
  val version: ScallopOption[Boolean] = opt[Boolean]("version", noshort = true, descr = "Print version")
  val help: ScallopOption[Boolean] = opt[Boolean]("help", noshort = true, descr = "Show this message")
  addSubcommand(play)
  addSubcommand(stop)
  verify()

  override def onError(e: Throwable): Unit = e match {
    case other => throw other
  }

}
