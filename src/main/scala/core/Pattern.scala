package core

import types._

import scala.reflect.runtime.universe._

object Pattern {

  def castToNumber[A: TypeTag](a: Any): A = {
    val casted = a match {
      case x: Integer =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: java.lang.Long =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: java.lang.Double =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: Seq[Any] =>
        typeOf[A] match {
          case t if t =:= typeOf[PatternValue[MidiValue]] =>
            x match {
              case Seq() => Chord(Seq())
              case Seq(head, tail @ _*) =>
                Chord(castToNumber[MidiValue](head) +: castToNumber[PatternValue[MidiValue]](tail).toSeq)
            }
          case t if t =:= typeOf[PatternValue[IntValue]] =>
            x match {
              case Seq() => Chord(Seq())
              case Seq(head, tail @ _*) =>
                Chord(castToNumber[IntValue](head) +: castToNumber[PatternValue[IntValue]](tail).toSeq)
            }
        }
      case x => throw new IllegalArgumentException(s"Unsupported type ${x.getClass.getName}")
    }
    val res = casted.asInstanceOf[A]
    res
  }

  /** Creates iterator which repeats given sequence in a loop
    *
    * @param sequence
    *   sequence of values to be repeated
    * @param repeat
    *   the number of repetitions defaults to 0. Set to -1 for Infinity
    * @param offset
    *   the position in sequence from which iterator starts (default 0)
    * @return
    *   iterator which generates sequence of values
    */
  def seq[A](sequence: Seq[A], repeat: Long = 1, offset: Int = 0): Iterator[A] = {
    val (h, t) =
      sequence.splitAt(if (offset < 0) sequence.length + offset else offset)
    Iterator.unfold((t ++ h, repeat * sequence.length)) {
      case (_, r) if r == 0 => None
      case (s, r)           => Some(s.head, (s.tail :+ s.head, r - 1))
    }
  }

  /** See Iterator trait and companion object for other useful functions/generators
    * https://www.scala-lang.org/api/current/scala/collection/Iterator$.html
    */
  implicit class IteratorOps[A](it: Iterator[A]) {
    def repeat(times: Int): Iterator[A] =
      Iterator
        .unfold[Iterator[A], (Iterator[A], Int)]((it, times)) {
          case (_, 0) => None
          case (it, count) =>
            val (a, b) = it.duplicate; Some((a, (b, count - 1)))
        }
        .flatten
  }

}
