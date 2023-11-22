package model

import core.Exception.EmptySeq
import core.Pattern.castToNumber
import core.{Interpreter, Utils}
import core.Types._

import scala.util.Try

case class TrackVersion(
    name: Option[String], // todo - required so it can be passed to command
    notesOn: String, // todo notesOn should be optional. Possible are scale notes and control change values
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

  def interpretIterator[A](code: String) = for {
    iter <- Interpreter.parseAndEval(code)
    unboxed = iter.asInstanceOf[Iterator[A]]
  } yield if (unboxed.isEmpty) throw EmptySeq(code) else unboxed

  def getTiming(implicit ppq: Int) = {
    interpretIterator[Double](timing)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter =>
        Iterator.unfold[Long, (Iterator[Double], Long)]((iter, offset.toTick)) {
          case (iterator, value) if iterator.hasNext =>
            val next = value + iterator.next().toTick
            Some((next, (iterator, next)))
          case _ => None
        }
      )
  }

  def getNote =
    interpretIterator[MidiValue](notesOn).map(iter => iter.map(castToNumber[MidiValue]))

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

  def getNoteEvents(implicit
      ppq: Int,
      channel: Channel = Channel(0)
  ): NoteEvents = {
    val zipped = for {
      notes <- getNote
      durations <- getDuration(ppq)
      timing <- getTiming(ppq)
      velocity <- getVelocity
    } yield notes zip durations zip timing zip velocity
    zipped.map(iter =>
      for {
        (((note, duration), timing), velocity) <- iter
        noteOn <- List(true, false)
      } yield {
        if (noteOn)
          NoteEvent(NoteOn(channel, note, velocity), timing)
        else
          NoteEvent(NoteOff(channel, note, velocity), timing + duration)
      }
    )
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
