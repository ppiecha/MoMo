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

  def synthesizer(): Resource[IO, Synthesizer] =
    Resource.make {
      for {
        synth <- IO(MidiSystem.getSynthesizer)
        _ <- IO(synth.open())
      } yield synth
    } { s =>
      IO.blocking(s.close()).handleError(_ => IO.unit)
    }

  def sequencer(synth: Synthesizer): Resource[IO, Sequencer] =
    Resource.make {
      for {
        seq <- IO.blocking(MidiSystem.getSequencer(false))
        _ <- IO(seq.getTransmitter.setReceiver(synth.getReceiver))
        _ <- IO(seq.open())
      } yield seq
    } { s =>
      (for {
        _ <- IO(s.close())
        _ <- logger.info("sequencer closed")
      } yield ()).handleError(_ => IO.unit)
    }

  def synthSequencer(): Resource[IO, (Synthesizer, Sequencer)] =
    for {
      synth <- synthesizer()
      seq <- sequencer(synth)
    } yield (synth, seq)

  private def waitWhilePlaying(sequencer: Sequencer): IO[Unit] = {
    for {
      _ <- IO.sleep(100 millisecond)
      _ <- if (sequencer.isRunning) waitWhilePlaying(sequencer) else IO.unit
    } yield ()
  }

  def play(sequence: Sequence, playOptions: PlayOptions): IO[Unit] =
    synthSequencer()
      .use {
        case (synth, sequencer) =>
          sequencer.setSequence(sequence)
          sequencer.setTempoInBPM(playOptions.BPM.toFloat)
          sequencer.start()
          waitWhilePlaying(sequencer)
      }
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
