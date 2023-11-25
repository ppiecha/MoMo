package model

import core.Pattern.castToNumber
import core.{Interpreter, Utils}

import types._

import javax.sound.midi.ShortMessage._
import scala.util.{Failure, Success, Try}

case class TrackVersion(
    name: String,
    scale: Option[String],
    scaleNote: Option[String],
    midiNote: Option[String],
    timing: String,
    start: Option[Double],
    duration: String,
    velocity: Option[String] = None,
    active: Option[Boolean] = Option(true)
) extends Playable {
  require(start.getOrElse(0.0) >= 0.0, s"Start $start should not be negative")

  implicit class DoubleOps(d: Double) {
    def toTick(implicit ppq: Int) = Utils.durToTick(d, ppq)
  }

  private val offset = start.getOrElse(0.0)

  def interpretIterator[A](code: String) =
    for {
      iter <- Interpreter.parseAndEval(code)
      unboxed = iter.asInstanceOf[Iterator[A]]
    } yield unboxed

  def getTiming(implicit ppq: Int) = {
    interpretIterator[Double](timing)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter =>
        Iterator.unfold[Long, (Iterator[Double], Long)]((iter, offset.toTick)) {
          case (iterator, value) if iterator.hasNext =>
            val next = value + iterator.next().toTick
            Some((next, (iterator, next)))
          case _ => None
      })
  }

  def getMidiNote = midiNote match {
    case Some(midiNote) =>
      interpretIterator[PatternValue[MidiValue]](midiNote)
        .map(iter => iter.map(castToNumber[PatternValue[MidiValue]]))
    case None => Try(Iterator.empty[PatternValue[MidiValue]])
  }

  def getDuration(ppq: Int) =
    interpretIterator[Double](duration)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter => iter.map(_.toTick(ppq)))

  def getVelocity = {
    if (velocity.getOrElse("").isEmpty) {
      // todo refactor velocity - default velocity on composition level
      Try(Iterator.iterate(MidiValue(100))(x => x))
    } else
      interpretIterator[MidiValue](velocity.get).map(iter => iter.map(castToNumber[MidiValue]))
  }

  def zippedIterators(implicit ppq: Int) =
    for {
      notes <- getMidiNote
      durations <- getDuration(ppq)
      timing <- getTiming(ppq)
      velocity <- getVelocity
    } yield {
      for ((((note, duration), timing), velocity) <- notes zip durations zip timing zip velocity)
        yield (note, duration, timing, velocity)
    }

  def getNoteEvents(implicit ppq: Int, channel: Channel = Channel(0)): Events = Try {
    zippedIterators match {
      case Failure(exception) => throw exception
      case Success(events) =>
        for {
          (note, duration, timing, velocity) <- events
          chordNote <- note.toSeq
          command <- List(NOTE_ON, NOTE_OFF)
        } yield {
          val tick = if (command == NOTE_OFF) timing + duration else timing
          NoteEvent(NoteMessage(MidiValue(command), channel, chordNote, velocity), tick)
        }
    }
  }

//  def getSequence(implicit ppq: Int, channel: Channel): Try[Sequence] = Try {
//    val sequence = new Sequence(Sequence.PPQ, ppq)
//    val track = sequence.createTrack()
//    getNoteEvents(ppq, channel) match {
//      case Failure(exception) => throw exception
//      case Success(iter) => iter.map(_.toMidiEvent).foreach(track.add)
//    }
//    sequence
//  }

}
