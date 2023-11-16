package core

object Utils {
  def durToTick(dur: Double, PPQ: Int): Long = dur match {
    case x if x == 0.0 => 0
    case _ => ((PPQ.toDouble * 4) / dur).toLong
  }
}
