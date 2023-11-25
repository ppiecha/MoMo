package types

import util._

/**
  * Events as iterators
  */
object Events {

  def fromSeqOfEvents(events: Seq[Event]): Events = Try(events.iterator)

  implicit class EventsOps(events: Events) {
    def ++(newEvents: Events): Events = mergeEvents(Seq(events, newEvents))
    def ++(newEvents: Seq[Event]): Events = mergeEvents(Seq(events, Events.fromSeqOfEvents(newEvents)))
  }

  def mergeEvents(events: Seq[Events]): Events =
    events.foldLeft[Events](Try(Iterator.empty[NoteEvent])) {
      case (Failure(exception), _)           => Failure(exception)
      case (_, Failure(exception))           => Failure(exception)
      case (Success(accIter), Success(iter)) => Success(accIter ++ iter)
    }

}
