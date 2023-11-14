package core

// TODO map template YAML to Composition case class

//import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
//import com.fasterxml.jackson.databind.ObjectMapper
//import com.fasterxml.jackson.module.scala.DefaultScalaModule
import io.circe.{Json, ParsingFailure}
import io.circe.yaml.parser
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import io.circe.yaml
import model.Composition

import java.io.FileReader
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
  //    val mapper = new ObjectMapper(new YAMLFactory())
//    mapper.registerModule(DefaultScalaModule)
//    mapper.readValue(fileReader, c)

}
