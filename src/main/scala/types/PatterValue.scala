package types

/** MIDI & Pattern values
  */
sealed trait InputValue
case class MidiValue(value: Int)(implicit c: MidiConstraint) extends InputValue {
  require(c.constraint(value), s"$value not in (0, 255) range")
}
case class IntValue(value: Int) extends InputValue

sealed trait PatternValue[A] {
  def toSeq: Seq[A] = this match {
    case SingleValue(s) => Seq(s)
    case Chord(chord)   => chord
  }
}
case class SingleValue[A](value: A) extends PatternValue[A]
case class Chord[A](chord: Seq[A]) extends PatternValue[A]
