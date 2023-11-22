package core

object Exception {

  case class EmptySeq(message: String) extends Exception(message)

}
