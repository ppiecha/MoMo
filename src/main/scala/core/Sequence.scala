package core

object Sequence {

  /** Creates iterator which repeats given sequence in a loop
   *
   *  @param sequence the values to be repeated
   *  @param repeat the number of repetitions - None for Infinity
   *  @param offset the position in sequence from which iterator starts (default 0)
   *  @return iterator which generates sequence of values
   */
  def seq[A](sequence: Seq[A], repeat: Option[Int] = None, offset: Int = 0): Iterator[A] = {
    val (h, t) = sequence.splitAt(offset)
    Iterator.unfold((t ++ h, repeat.map(_ * sequence.length))) {
      case (_, r) if r.getOrElse(-1) == 0 => None
      case (s, r) => Some(s.head, (s.tail :+ s.head, r.map(_ - 1)))
    }
  }

}
