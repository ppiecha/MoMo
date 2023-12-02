package core

import io.circe.yaml.parser
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import model.Composition
import cats.effect._

object Yaml {
  def mapYaml(yamlString: String): IO[Composition] = {
    val json = parser.parse(yamlString)
    IO {
      json
        .leftMap(err => err: Error)
        .flatMap(_.as[Composition])
        .valueOr(throw _)
    }
  }
}
