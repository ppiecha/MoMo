package core

import scala.util.Try

object Interpreter {

  private val toolbox = locally {
    import scala.tools.reflect.ToolBox
    reflect.runtime.currentMirror.mkToolBox()
  }

  def addImports(code: String) = Seq("import core.Sequence._", code).mkString("\n")

  def parse(code: String): Try[Types.InterpreterTree] = Try(toolbox.parse(addImports(code)))

  def eval(tree: Types.InterpreterTree) = Try(toolbox.eval(tree))

  def parseAndEval(code: String): Try[Any] =
    for {
      parsed <- parse(code)
      value <- eval(parsed)
    } yield value//.asInstanceOf[A]

}
