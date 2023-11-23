package core

import scala.util.{Failure, Success, Try}

object Types {

  sealed trait Event {
    def message: Message
    def tick: Long
  }
  case class NoteEvent(message: NoteMessage, tick: Long) extends Event
  case class ProgramEvent(message: ProgramMessage, tick: Long) extends Event
  case class ControlEvent(message: ControlMessage, tick: Long) extends Event

  sealed trait Message
  case class NoteMessage(command: MidiValue, channel: Channel, note: MidiValue, velocity: MidiValue) extends Message
  case class ProgramMessage(channel: Channel, bank: MidiValue, program: MidiValue) extends Message
  case class ControlMessage(channel: Channel, control: MidiValue, value: MidiValue) extends Message

  type InterpreterTree = scala.reflect.runtime.universe.Tree

  trait Constrained[A] {
    def constraint(value: A): Boolean
  }

  type Events = Try[Iterator[Event]]

  object Events {
    def fromSeqOfEvents(events: Seq[Event]): Events = Try(events.iterator)
  }

  implicit class EventsOps(events: Events) {

    def ++(newEvents: Events): Events = mergeEvents(Seq(events, newEvents))

    def ++(newEvents: Seq[Event]): Events = mergeEvents(Seq(events, Events.fromSeqOfEvents(newEvents)))

  }

  trait Playable {
    def getNoteEvents(implicit ppq: Int, channel: Channel = Channel(0)): Events
  }

  def mergeEvents(events: Seq[Events]): Events =
    events.foldLeft[Events](Try(Iterator.empty[Types.NoteEvent])) {
      case (Failure(exception), _)           => Failure(exception)
      case (_, Failure(exception))           => Failure(exception)
      case (Success(accIter), Success(iter)) => Success(accIter ++ iter)
    }

  trait MidiConstraint extends Constrained[Int]

  implicit val midiConstraint: MidiConstraint =
    (value: Int) => (0 until 256) contains value

  case class MidiValue(value: Int)(implicit c: MidiConstraint) {
    require(c.constraint(value), s"$value not in (0, 255) range")
  }

  trait ChannelConstraint extends Constrained[Int]

  implicit val channelConstraint: ChannelConstraint =
    (value: Int) => (0 until 16) contains value

  case class Channel(number: Int)(implicit c: ChannelConstraint) {
    require(c.constraint(number), s"Channel $number not in (0, 16) range")
  }

  // Numbers

  trait PositiveConstraint[A] extends Constrained[A]

  implicit def positiveConstraint[A](implicit n: Numeric[A]): PositiveConstraint[A] =
    (value: A) => n.compare(value, n.zero) > 0

  case class Positive[A](value: A)(implicit c: PositiveConstraint[A]) {
    require(c.constraint(value), s"$value is not positive")
  }

}
