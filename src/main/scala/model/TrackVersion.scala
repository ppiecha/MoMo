package model

import core.Sequence.castToNumber
import core.{Interpreter, Utils}
import core.Types._
import scala.util.Try

// todo program workflow as sequencer interface (play, stop etc...)
// todo notesOn should be optional. Possible are scale notes and control change values
// todo velocity
// todo add chords as tuples
// todo add readme and github description

case class TrackVersion(
  name: Option[String],
  notesOn: String,
  timing: String,
  start: Option[Double],
  duration: String,
  velocity: Option[String] = None,
  active: Option[Boolean] = Option(true)
) {
  require(start.getOrElse(0.0) >= 0.0, s"Start $start should not be negative")

  implicit class DoubleOps(d: Double) {
    def toTick(implicit ppq: Int) = Utils.durToTick(d, ppq)
  }

  def withDefaults() = this.copy(
    start = if (start.isEmpty) Option(0.0) else start,
    active = if (active.isEmpty) Option(true) else active
  )

  private val offset = start.getOrElse(0.0)

  def interpretIterator[A](code: String) = for {
    iter <- Interpreter.parseAndEval(code)
    unboxed = iter.asInstanceOf[Iterator[A]]
  } yield unboxed

  def getTiming(implicit ppq: Int) = {
    interpretIterator[Double](timing)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter => Iterator.unfold[Long, (Iterator[Double], Long)]((iter, offset.toTick)) {
        case (iterator, value) if iterator.hasNext =>
          val next = value + iterator.next().toTick
          Some((next, (iterator, next)))
        case _ => None
      })
  }

  def getMidiNote() =
    interpretIterator[MidiValue](notesOn).map(iter => iter.map(castToNumber[MidiValue]))

  def getDuration(implicit ppq: Int) =
    interpretIterator[Double](duration)
      .map(iter => iter.map(castToNumber[Double]))
      .map(iter => iter.map(_.toTick))


  def getVelocity() =
    Try(Iterator.iterate(MidiValue(100))(x => x))


  def getEvents(implicit ppq: Int, channel: Channel) = {
    val zipped = for {
      notes <- getMidiNote()
      durations <- getDuration(ppq)
      timing <- getTiming(ppq)
      velocity <- getVelocity()
    } yield notes zip durations zip timing zip velocity
    zipped.map(iter => for {
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
}


