package console
import types._

import javax.sound.midi._
import cats.effect._
import cats.effect.std.Supervisor
import console.AppState.{handler, log}

import scala.concurrent.duration.DurationInt
import java.io.File

object Player {

  private def synthesizer(soundFile: SoundFile): Resource[IO, Synthesizer] =
    Resource.make {
      for {
        synth <- IO(MidiSystem.getSynthesizer)
        _ <- IO(synth.open())
        _ <- log("start font")
        _ <- readSoundFile(synth, soundFile).handleErrorWith(handler(None))
        _ <- log("stop font")
      } yield synth
    } { s =>
      IO(s.close()).handleError(_ => IO.unit)
    }

  private def sequencer(synth: Synthesizer): Resource[IO, Sequencer] =
    Resource.make {
      for {
        seq <- IO(MidiSystem.getSequencer(false))
        _ <- IO(seq.getTransmitter.setReceiver(synth.getReceiver))
        _ <- IO(seq.open())
      } yield seq
    } { s =>
      IO(s.close()).handleError(_ => IO.unit)
    }

  private def synthSequencer(soundFile: SoundFile): Resource[IO, (Synthesizer, Sequencer)] =
    for {
      synth <- synthesizer(soundFile)
      seq <- sequencer(synth)
    } yield (synth, seq)

  private def synthSequencer2(soundFile: SoundFile): Resource[IO, (Synthesizer, Sequencer)] =
    for {
      synth <- synthesizer(soundFile)
      seq <- sequencer(synth)
    } yield (synth, seq)

  def waitWhilePlaying(sequencer: Sequencer): IO[Unit] = {
    for {
      _ <- IO.sleep(100 millisecond)
      //_ <- log("waitWhilePlaying")
      _ <- if (sequencer.isRunning) waitWhilePlaying(sequencer) else IO.unit
      //_ <- log("waitWhilePlaying")
    } yield ()
  }


  def play(sequence: Sequence, playOptions: PlayOptions) =
    synthSequencer(playOptions.soundFile)
      .use {
        case (_, sequencer) =>
          sequencer.setSequence(sequence)
          sequencer.setTempoInBPM(playOptions.BPM.toFloat)
          sequencer.start()
          waitWhilePlaying(sequencer)
      }

  /** play logic
   * create supervisor
   * in it use synthSequencer to load in background (supervise) sound font
   * in main thread prepare sequence
   * once both are ready start player and wait in supervise
   */

  private def readSoundFile(synth: Synthesizer, soundFile: SoundFile): IO[Unit] = soundFile match {
    case Some(file) =>
      for {
        //fileExists <- IO(file.exists())
        soundBank <- IO(MidiSystem.getSoundbank(file))
        isValid <- IO.pure(synth.isSoundbankSupported(soundBank))
        _ <- if (isValid) IO(synth.loadAllInstruments(soundBank)) else IO.pure(false)
        //_ <- log(s"Using soundbank ${soundBank.getName}")
      } yield ()
    case None => IO.unit
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
