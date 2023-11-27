package types

import util._

/**
  * Events as iterators
  */
object Events {

  def fromSeqOfEvents(events: Seq[Event]): Events[Event] = Try(events.iterator)

  implicit class EventsOps(events: Events[Event]) {
    def ++(newEvents: Events[Event]): Events[Event] = mergeEvents(Seq(events, newEvents))
    def ++(newEvents: Seq[Event]): Events[Event] = mergeEvents(Seq(events, Events.fromSeqOfEvents(newEvents)))
  }

  def mergeEvents(events: Seq[Events[Event]]): Events[Event] =
    events.foldLeft[Events[Event]](Try(Iterator.empty[NoteEvent])) {
      case (Failure(exception), _)           => Failure(exception)
      case (_, Failure(exception))           => Failure(exception)
      case (Success(accIter), Success(iter)) => Success(accIter ++ iter)
    }

}
