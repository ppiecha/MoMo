package model

import core.Exception.ArgError
import core.Pattern.castToNumber
import core.Utils.tickToSecond
import core.{Interpreter, Utils}
import types._

import javax.sound.midi.ShortMessage._
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

case class TrackVersion(
    name: String,
    scale: Option[Scale],
    scaleNote: Option[String],
    midiNote: Option[String],
    timing: String,
    startAt: Option[Double],
    duration: String,
    velocity: Option[String] = None,
    active: Option[Boolean] = Option(true)
) extends Playable {
  require(startAt.getOrElse(0.0) >= 0.0, s"$name. Start $startAt should not be negative")
  require(!name.isBlank, s"$name. Name can't be empty")
  require(!timing.isBlank, s"$name. Timing can't be empty")
  require(!duration.isBlank, s"$name. Duration can't be empty")

  implicit class DoubleOps(d: Double) {
    def toTick(implicit ppq: Int): Long = Utils.durToTick(d, ppq)
  }

  implicit val versionName: String = name

  private val offset = startAt.getOrElse(0.0)

  def interpretIterator[A](code: String)(implicit versionName: String): Events[A] = {
    val tryParse =
      for {
        iter <- Interpreter.parseAndEval(code)
        unboxed = iter.asInstanceOf[Iterator[A]]
      } yield unboxed
    tryParse match {
      case Failure(exception) =>
        throw new RuntimeException(s"Version ===== $versionName ===== ${exception.getMessage}")
      case Success(_) => tryParse
    }
  }

  def getTiming(implicit ppq: Int): Events[Long] = {
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

  def getNote[A <: InputValue](pattern: String)(implicit tag: TypeTag[A]): Events[PatternValue[InputValue]] = {
    if (pattern.isBlank) throw ArgError("Note sequence is empty")
    interpretIterator[PatternValue[A]](pattern)
      .map(iter =>
        typeOf[A] match {
          case t if t =:= typeOf[IntValue]  => iter.map(castToNumber[PatternValue[IntValue]])
          case t if t =:= typeOf[MidiValue] => iter.map(castToNumber[PatternValue[MidiValue]])
      })
  }

  def getNote: Events[PatternValue[InputValue]] = (scaleNote, midiNote) match {
    case (None, None)            => Failure(ArgError("Notes not defined"))
    case (Some(scaleNote), None) => getNote[IntValue](scaleNote)
    case (None, Some(midiNote))  => getNote[MidiValue](midiNote)
    case (Some(_), Some(_)) =>
      Failure(ArgError("Either scaleNote or midiNote must be defined at the same time"))
  }

  def getDuration(ppq: Int): Events[Long] =
    interpretIterator[Double](duration)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter => iter.map(_.toTick(ppq)))

  def getVelocity: Events[MidiValue] = {
    if (velocity.getOrElse("").isEmpty) {
      // todo refactor velocity - default velocity on composition level
      Try(Iterator.iterate(MidiValue(100))(x => x))
    } else
      interpretIterator[MidiValue](velocity.get).map(iter => iter.map(castToNumber[MidiValue]))
  }

  def zippedIterators(implicit
                      ppq: Int,
                      scale: Option[Scale]): Events[(PatternValue[InputValue], Long, Long, MidiValue)] =
    for {
      notes <- getNote
      durations <- getDuration(ppq)
      timing <- getTiming(ppq)
      velocity <- getVelocity
    } yield {
      for ((((note, duration), timing), velocity) <- notes zip durations zip timing zip velocity)
        yield (note, duration, timing, velocity)
    }

  def getNoteEvents(implicit opt: PlayOptions, channel: Int = 0): Events[Event] = Try {
    implicit val scale: Option[Scale] = this.scale
    (zippedIterators(opt.PPQ, scale) match {
      case Failure(exception) => throw exception
      case Success(events) =>
        for {
          (note, duration, timing, velocity) <- events
          chordNote <- note.toSeq.map(_.toMidi)
          if duration > 0 && velocity > 0
          command <- List(NOTE_ON, NOTE_OFF)
        } yield {
          val tick = if (command == NOTE_OFF) timing + duration else timing
          NoteEvent(NoteMessage(command, channel, chordNote, velocity), tick)
        }
    }).takeWhile(e => tickToSecond(e.tick, opt.PPQ, opt.BPM) < opt.LENGTH_LIMIT)
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
