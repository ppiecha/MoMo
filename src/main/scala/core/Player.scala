package core

import com.typesafe.scalalogging.Logger
import core.Types.Channel
import model.TrackVersion

import java.io.File
import javax.sound.midi.{MidiEvent, MidiSystem, Sequence, Sequencer, ShortMessage, Synthesizer}

object Player {

  private val logger = Logger(getClass.getName)

  val synth: Synthesizer = MidiSystem.getSynthesizer
  synth.open()
  synth.unloadAllInstruments(synth.getDefaultSoundbank)
  synth.loadAllInstruments(MidiSystem.getSoundbank(new File("soundfont.sf2")))

  val sequencer: Sequencer = MidiSystem.getSequencer(false)
  sequencer.getTransmitter.setReceiver(synth.getReceiver)


  def makeEvent(command: Int, channel: Int, note: Int, velocity: Int, tick: Int) = {
    val msg = new ShortMessage()
    msg.setMessage(command, channel, note, velocity)
    new MidiEvent(msg, tick)
  }

  def playTrackVersion(version: TrackVersion)(implicit ppq: Int, channel: Channel) = {
    val sequence = new Sequence(Sequence.PPQ, ppq)
    val track = sequence.createTrack()
    val events = version.getEvents.map(_.toSeq)
    events.map(seq => seq.foreach(e => track.add(makeEvent(
      e.noteMessage.command.value,
      e.noteMessage.channel.number,
      e.noteMessage.note.value,
      e.noteMessage.velocity.value,
      e.tick.toInt
    ))))
    logger.info(s"Events $events")
    sequencer.open()
    sequencer.setSequence(sequence)
    sequencer.setTempoInBPM(220)
    sequencer.start()
    logger.info("Playing...")
    while (true) {
      // Exit the program when sequencer has stopped playing.
      if (!sequencer.isRunning) {
        sequencer.close()
        //System.exit(1)
      }
    }
    logger.info("Sequencer closed")
    events
  }

}
