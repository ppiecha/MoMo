package core

import scala.util.Try

object Interpreter {

  private val toolbox = locally {
    import scala.tools.reflect.ToolBox
    reflect.runtime.currentMirror.mkToolBox()
  }

  def parse(code: String): Try[Types.InterpreterTree] = Try(toolbox.parse(code))

  def eval(tree: Types.InterpreterTree) = Try(toolbox.eval(tree))

  def parseAndEval[A](code: String): Try[A] =
    for {
      parsed <- parse(code)
      value <- eval(parsed)
    } yield value.asInstanceOf[A]

}
