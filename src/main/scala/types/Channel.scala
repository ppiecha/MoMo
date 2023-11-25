package types

trait ChannelConstraint extends Constrained[Int]

case class Channel(number: Int)(implicit c: ChannelConstraint) {
  require(c.constraint(number), s"Channel $number not in (0, 16) range")
}
