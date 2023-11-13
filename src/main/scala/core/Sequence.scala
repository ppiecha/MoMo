package core

object Sequence extends App {

  // TODO add ser (start stop step) supporting doubles

  /** Creates iterator which repeats given sequence in a loop
   *
   * @param sequence the values to be repeated
   * @param repeat the number of repetitions defaults to 0. Set to -1 for Infinity
   * @param offset the position in sequence from which iterator starts (default 0)
   * @return iterator which generates sequence of values
   */
  def seq[A](sequence: Seq[A], repeat: Long = 1, offset: Int = 0): Iterator[A] = {
    val (h, t) = sequence.splitAt(if (offset < 0) sequence.length + offset else offset)
    Iterator.unfold((t ++ h, repeat * sequence.length)) {
      case (_, r) if r == 0 => None
      case (s, r) => Some(s.head, (s.tail :+ s.head, r - 1))
    }
}

  /** See Iterator trait and companion object for other useful functions/generators
   * https://www.scala-lang.org/api/current/scala/collection/Iterator$.html
   */


  /** Empty seq */
  def emptySeq[A]: Iterator[A] = Iterator.empty[A]

  implicit class IteratorOps[A](it: Iterator[A]) {
    def repeat(times: Int): Iterator[A] =
      Iterator.unfold[Iterator[A], (Iterator[A], Int)]((it, times)){
        case (_, 0) => None
        case (it, count) => val (a, b) = it.duplicate; Some((a, (b, count - 1)))
      }.flatten
  }
//  println(seq(Seq(1, 2)).take(20).toSeq)
//  println(seq(Seq(1, 2), -1).take(20).toSeq)
//  println(seq(Seq(1, 2)).repeat(0).take(20).toSeq)
//  println(seq(Seq(1, 2)).repeat(1).take(20).toSeq)
//  println(seq(Seq(1, 2)).repeat(2).take(20).toSeq)
//  println(seq(Seq(1, 2)).repeat(-1).take(20).toSeq)
}
