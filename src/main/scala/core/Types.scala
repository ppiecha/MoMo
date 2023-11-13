package core

object Types extends App {
  type InterpreterTree = Interpreter.toolbox.u.Tree

  trait Constrained[A] {
    def constraint(value: A): Boolean
  }

  // Midi

  type TimeSeq = Iterator[Int]

  type MessageSeq[A] = Iterator[A]

  type EventSeq[A] = (MessageSeq[A], TimeSeq)

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
