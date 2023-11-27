package console

import com.typesafe.scalalogging.Logger
import types._

import java.io.File
import javax.sound.midi._
import scala.util.{Failure, Success, Try}

object Player {

  private val logger = Logger(getClass.getName)

  private var soundPath: Option[String] = None

  private type Synth = Try[Synthesizer]

  implicit val synth: Synth = Try {
    val synth = MidiSystem.getSynthesizer
    if (!synth.isOpen)
      synth.open()
    synth
  }

  case class SoundFont(pathName: Option[String])(implicit synth: Synth) {
    def getSoundBank: Try[Option[Soundbank]] = pathName match {
      case Some(pathName) =>
        val defaultSoundFont = os.pwd / "soundfonts" / pathName
        val filePath = if (os.exists(defaultSoundFont)) defaultSoundFont.toString() else pathName
        Try(MidiSystem.getSoundbank(new File(filePath))) match {
          case Failure(exception) => Failure(exception)
          case Success(soundBank) =>
            synth.map(s =>
              s.isSoundbankSupported(soundBank) match {
                case true  => Some(soundBank)
                case false => throw new IllegalArgumentException(s"Soundfont not supported $pathName")
            })
        }
      case None =>
        synth match {
          case Failure(exception) => Failure(exception)
          case Success(synth)     => Success(Option(synth.getDefaultSoundbank))
        }
    }
  }

  def reloadSoundBank(pathName: Option[String])(implicit synth: Synth): Synth = {
    if (pathName.isEmpty || pathName == soundPath) {
      logger.warn(s"Soundbank reload discarded. Soundpath $soundPath")
      synth
    } else
      synth match {
        case Failure(exception) => Failure(exception)
        case Success(synthesizer) =>
          (SoundFont(pathName).getSoundBank, SoundFont(soundPath).getSoundBank) match {
            case (Failure(exception), _) => Failure(exception)
            case (_, Failure(exception)) => Failure(exception)
            case (Success(next), Success(prev)) =>
              next match {
                case None => synth
                case Some(soundBank) =>
                  if (prev.nonEmpty)
                    synthesizer.unloadAllInstruments(prev.get)
                  if (synthesizer.loadAllInstruments(soundBank)) {
                    soundPath = pathName
                    logger.info(s"Soundbank ${soundBank.getName}")
                  }
                  synth
              }
          }
      }
  }

  def getSequencer: Try[Sequencer] = {
    Try(MidiSystem.getSequencer(false)) match {
      case Failure(exception) => Failure(exception)
      case Success(sequencer) =>
        synth match {
          case Failure(exception) => Failure(exception)
          case Success(synth) =>
            sequencer.getTransmitter.setReceiver(synth.getReceiver)
            sequencer.addControllerEventListener(
              (event: ShortMessage) =>
                logger.info(
                  s"Controller Event Listener. " +
                    s"Channel ${event.getChannel} " +
                    s"command ${event.getCommand} " +
                    s"data1 ${event.getData1} " +
                    s"data2 ${event.getData1}"
              ),
              Array.range(0, 128)
            )
            sequencer.addMetaEventListener((meta: MetaMessage) => if (meta.getType == 0x2f) sequencer.close())
            Success(sequencer)
        }
    }
  }

  def close(): Try[Unit] =
    for {
      sequencer <- getSequencer
      synth <- synth
    } yield {
      sequencer.close()
      synth.close()
      println("Sequencer closed")
    }

  def play(p: Playable)(implicit opt: PlayOptions): Try[Sequence] =
    for {
      sequencer <- getSequencer
      sequence <- MidiSequence.fromNoteEvents(p.getNoteEvents)
    } yield {
      sequencer.open()
      sequencer.setSequence(sequence)
      sequencer.setTempoInBPM(opt.BPM)
      sequencer.start()
      sequence
    }

  def stop(): Try[Unit] = getSequencer.map(_.stop())

}
