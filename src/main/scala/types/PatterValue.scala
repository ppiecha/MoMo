package types

import core.Exception.ArgError

/** MIDI & Pattern values
  */
sealed trait InputValue {
  def toMidi(implicit scale: Option[Scale]): MidiValue
}
case class MidiValue(value: Int)(implicit c: MidiConstraint) extends InputValue {
  require(c.constraint(value), s"$value not in (0, 255) range")
  def toMidi(implicit scale: Option[Scale]): MidiValue = this
}
case class IntValue(value: Int) extends InputValue {
  def toMidi(implicit scale: Option[Scale]): MidiValue = scale match {
    case Some(scale) => scale.toMidiValue(value)
    case None        => throw ArgError("Scale not defined")
  }
}
case class FunValue(fun: () => Int) extends InputValue {
  def toMidi(implicit scale: Option[Scale]): MidiValue = MidiValue(fun())
}

sealed trait PatternValue[+A <: InputValue] {
  def toSeq: Seq[A] = this match {
    case SingleValue(s) => Seq(s)
    case Chord(chord)   => chord
  }
  def toMidi(implicit scale: Option[Scale]): PatternValue[MidiValue] = this match {
    case SingleValue(value) => SingleValue(value.toMidi)
    case Chord(chord)       => Chord(chord.map(_.toMidi))
  }
}
case class SingleValue[A <: InputValue](value: A) extends PatternValue[A]
case class Chord[A <: InputValue](chord: Seq[A]) extends PatternValue[A]
