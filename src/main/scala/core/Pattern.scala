package core

import types._

import scala.reflect.runtime.universe._
import scala.util.Random.shuffle

object Pattern {

  def castToNumber[A: TypeTag](a: Any): A = {
    val casted = a match {
      case x: Integer =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[IntValue]                => IntValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: java.lang.Long =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[IntValue]                => IntValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: java.lang.Double =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x.toDouble
          case t if t =:= typeOf[Int]                     => x.toInt
          case t if t =:= typeOf[Long]                    => x.toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x.toInt)
          case t if t =:= typeOf[IntValue]                => IntValue(x.toInt)
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x.toInt))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x.toInt))
        }
      case x: Seq[Any] =>
        typeOf[A] match {
          case t if t =:= typeOf[PatternValue[MidiValue]] =>
            x match {
              case Seq() => Chord[MidiValue](Seq())
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
      case x: (() => Int) =>
        typeOf[A] match {
          case t if t =:= typeOf[Double]                  => x().toDouble
          case t if t =:= typeOf[Int]                     => x()
          case t if t =:= typeOf[Long]                    => x().toLong
          case t if t =:= typeOf[MidiValue]               => MidiValue(x())
          case t if t =:= typeOf[IntValue]                => IntValue(x())
          case t if t =:= typeOf[PatternValue[MidiValue]] => SingleValue(MidiValue(x()))
          case t if t =:= typeOf[PatternValue[IntValue]]  => SingleValue(IntValue(x()))
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
    *   the number of repetitions defaults to 1. Set to -1 for Infinity
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

  /** Creates iterator which return number of items instead of the number of complete cycles
    *
    * @param sequence
    *   sequence of values to be repeated
    * @param length
    *   the number of items to be returned. Set to -1 for Infinity
    * @param offset
    *   the position in sequence from which iterator starts (default 0)
    * @return
    *   iterator which generates sequence of values
    */
  def ser[A](sequence: Seq[A], length: Long = 1, offset: Int = 0): Iterator[A] = sequence match {
    case Nil => Iterator.empty[A]
    case _ =>
      val (h, t) = sequence.splitAt(if (offset < 0) sequence.length + offset else offset)
      Iterator.unfold((t ++ h, length)) {
        case (_, l) if l == 0 => None
        case (s, l)           => Some(s.head, (s.tail :+ s.head, l - 1))
      }
  }

  /** Creates iterator which return number of items instead of the number of complete cycles
    *
    * @param start
    *   start number (can be Double)
    * @param step
    *   negative or positive step of sequence
    * @param length
    *   the number of items to be returned. Set to -1 for Infinity
    * @return
    *   iterator which generates sequence of values
    */
  def from[A](start: A, step: A, length: Long = 1)(implicit num: Numeric[A]): Iterator[A] = {
    Iterator.unfold[A, (A, Long)]((start, length)) {
      case (_, l) if l == 0 => None
      case (s, l)           => Some(s, (num.plus(s, step), l - 1))
    }
  }

  /** Selects item from sequence randomly
    *
    * @param sequence
    *   sequence to get value from
    * @param length
    *   the number of items to be returned. Set to -1 for Infinity
    * @return
    *   randomly chosen item from the sequence
    */
  def rnd[A](sequence: Seq[A]): () => A = () => shuffle(sequence).head

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
