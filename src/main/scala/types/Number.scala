package types

/**
  * Numbers
  */
trait PositiveConstraint[A] extends Constrained[A]

case class Positive[A](value: A)(implicit c: PositiveConstraint[A]) {
  require(c.constraint(value), s"$value is not positive")
}
