package core

import com.typesafe.scalalogging.Logger
import types._

import scala.util.Try

object Interpreter {

  private val logger = Logger(getClass.getName)

  private val toolbox = locally {
    import scala.tools.reflect.ToolBox
    reflect.runtime.currentMirror.mkToolBox()
  }

  def addImports(code: String) = Seq("import core.Pattern._", code).mkString("\n")

  def parse(code: String): Try[InterpreterTree] = Try(toolbox.parse(addImports(code)))

  def eval(tree: InterpreterTree) = Try(toolbox.eval(tree))

  def parseAndEval(code: String): Try[Any] = {
    logger.info(s"Parsing and evaluating $code")
    for {
      parsed <- parse(code)
      value <- eval(parsed)
    } yield value
  }

}
