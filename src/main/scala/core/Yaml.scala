package core

import io.circe.yaml.parser
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import model.Composition

import scala.util.Try

object Yaml {
  def mapYaml(yamlString: String) = {
    val json = parser.parse(yamlString)
    Try {
      json
        .leftMap(err => err: Error)
        .flatMap(_.as[Composition])
        .valueOr(throw _)
    }
  }
}
