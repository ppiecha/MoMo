import scala.util.Try

package object types {
  type Events = Try[Iterator[Event]]
  type InterpreterTree = scala.reflect.runtime.universe.Tree

  trait Constrained[A] { def constraint(value: A): Boolean }
  trait MidiConstraint extends Constrained[Int]
  implicit val channelConstraint: ChannelConstraint =
    (value: Int) => (0 until 16) contains value
  implicit def positiveConstraint[A](implicit n: Numeric[A]): PositiveConstraint[A] =
    (value: A) => n.compare(value, n.zero) > 0
  implicit val midiConstraint: MidiConstraint =
    (value: Int) => (0 until 256) contains value
}
