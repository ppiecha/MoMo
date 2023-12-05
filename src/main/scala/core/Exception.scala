package core

object Exception {
  case class ArgError(message: String) extends IllegalArgumentException(message)
}
