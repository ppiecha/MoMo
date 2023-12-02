package console

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import types._

import javax.sound.midi._
import cats.effect._

import scala.concurrent.duration.DurationInt

object Player {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private var soundPath: Option[String] = None

  type StopEffect = Option[Sequencer => Unit]

  //private type Synth = Synthesizer

//
//  val s = for {
//    synth <- synth
//  } yield synth

//  implicit val synth: Synth = {
//    val s = MidiSystem.getSynthesizer
//    s.open()
//    println("synth " + s.toString)
//    s
//  }

//    for {
//    synthesizer <- IO(MidiSystem.getSynthesizer)
//    _ <- logger.info("synthesizer " + synthesizer.toString)
//    _ <- IO(synthesizer.open())
//  } yield synthesizer

//  val sequencer = {
//    val sequencer = MidiSystem.getSequencer(false)
//    // s <- synth
//    // _ <- IO(sequencer.getTransmitter.setReceiver(synth.getReceiver))
//    // _ <- IO(sequencer.open())
//    sequencer.getTransmitter.setReceiver(synth.getReceiver)
//    sequencer.open()
//    println("sequencer " + sequencer.toString)
//    sequencer
//  }

  def synthesizer(): Resource[IO, Synthesizer] =
    Resource.make(IO.blocking(MidiSystem.getSynthesizer)) { s =>
      IO.blocking(s.close()).handleError(_ => IO.unit)
    }

  def sequencer(): Resource[IO, Sequencer] =
    Resource.make(IO.blocking(MidiSystem.getSequencer(false))) { s =>
      IO.blocking(s.close()).handleError(_ => IO.unit)
    }

  def synthSequencer(): Resource[IO, (Synthesizer, Sequencer)] =
    for {
      synth <- synthesizer()
      seq <- sequencer()
    } yield (synth, seq)

  def play(p: Playable)(implicit opt: PlayOptions): IO[Unit] =
    synthSequencer()
      .use {
        case (synth, sequencer) =>
          for {
            _ <- logger.info(s"synthSequencer ${synth.toString} ${sequencer.toString}")
            _ <- IO.blocking(synth.open())
            _ <- IO.blocking(sequencer.getTransmitter.setReceiver(synth.getReceiver))
            _ <- IO.blocking(sequencer.open())
            _ <- IO.blocking(sequencer.setSequence(MidiSequence.fromNoteEvents(p.getNoteEvents)))
            _ <- IO.blocking(sequencer.setTempoInBPM(opt.BPM.toFloat))
            _ <- IO.blocking(sequencer.start())
            _ <- logger.info("playing")
            //_ <- IO.sleep(5 second)
            _ <- waitWhilePlaying(sequencer)
            _ <- logger.info("stopped playing")
          } yield ()
      }
      .handleErrorWith(e => logger.error(e.getMessage))

  private def waitWhilePlaying(sequencer: Sequencer): IO[Unit] =
    for {
      _ <- IO.sleep(100 millisecond)
      _ <- if (sequencer.isRunning) waitWhilePlaying(sequencer) else IO.unit
    } yield ()

  //  case class SoundFont(pathName: Option[String])(implicit synth: Synth) {
  //    def getSoundBank(): IO[Option[Soundbank]] = pathName match {
  //      case Some(pathName) =>
  //        val defaultSoundFont = os.pwd / "soundfonts" / pathName
  //        for {
  //          filePath <- IO(if (os.exists(defaultSoundFont)) defaultSoundFont.toString() else pathName)
  //          soundBank <- IO(MidiSystem.getSoundbank(new File(filePath)))
  //          s <- synth
  //        } yield
  //          if (s.isSoundbankSupported(soundBank)) Some(soundBank)
  //          else throw new IllegalArgumentException(s"Soundfont not supported $pathName")
  //      case None => synth.map(s => Option(s.getDefaultSoundbank))
  //    }
  //  }

  //  def reloadSoundBank(pathName: Option[String])(implicit synth: Synth): Synth =
  //    if (pathName.isEmpty || pathName == soundPath) {
  //      for {
  //        _ <- logger.warn(s"SoundBank reload discarded. Soundpath $soundPath")
  //        s <- synth
  //      } yield s
  //    } else {
  //      for {
  //        s <- synth
  //        nextBank <- SoundFont(pathName).getSoundBank()
  //        _ <- logger.info("after  nextBank")
  //        prevBank <- SoundFont(soundPath).getSoundBank()
  //        _ <- logger.info("after  prevBank")
  //        _ <- IO {
  //          nextBank match {
  //            case None => s
  //            case Some(soundBank) =>
  //              if (prevBank.nonEmpty)
  //                s.unloadAllInstruments(prevBank.get)
  //              if (s.loadAllInstruments(soundBank)) {
  //                soundPath = pathName
  //                logger.info(s"SoundBank ${soundBank.getName}")
  //              }
  //          }
  //        }
  //      } yield s
  //    }

}
