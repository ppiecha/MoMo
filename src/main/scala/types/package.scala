import core.Exception.ArgError

import scala.language.implicitConversions
import scala.util.Try

package object types {

  val DEFAULT_BPM = 120
  val DEFAULT_PPQ = 480
  val DEFAULT_LENGTH_LIMIT = 600

  type Events[A] = Try[Iterator[A]]
  type InterpreterTree = scala.reflect.runtime.universe.Tree

  trait Constrained[A] { def constraint(value: A): Boolean }
  trait MidiConstraint extends Constrained[Int]
  implicit def positiveConstraint[A](implicit n: Numeric[A]): PositiveConstraint[A] =
    (value: A) => n.compare(value, n.zero) > 0
  implicit val midiConstraint: MidiConstraint =
    (value: Int) => (0 until 256) contains value

  trait Channel {
    def channel: Int
    def validateChannel(): Unit =
      if (!((0 until 16) contains channel))
        throw ArgError(s"Channel $channel not in (0, 16) range")
  }

  implicit def intToMidi(int: Int): MidiValue = MidiValue(int)
  implicit def midiToInt(midiValue: MidiValue): Int = midiValue.value

}
