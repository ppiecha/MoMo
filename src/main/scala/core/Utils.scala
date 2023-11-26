package core

object Utils {

  def durToTick(dur: Double, PPQ: Int): Long = dur match {
    case x if x == 0.0 => 0
    case _             => ((PPQ.toDouble * 4) / dur).toLong
  }

  def tickPerSecond(PPQ: Int, BPM: Int): Double = PPQ * (BPM / 60.0)

  def tickToSecond(tick: Long, PPQ: Int, BPM: Int): Double = tick / tickPerSecond(PPQ, BPM)

}
