ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.10",
  "org.scala-lang" % "scala-compiler" % "2.13.10",
  "org.scalactic" %% "scalactic" % "3.2.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-classic" % "1.4.7",
  "io.circe" %% "circe-yaml" % "0.14.2",
  "io.circe" %% "circe-core" % "0.14.5",
  "io.circe" %% "circe-generic" % "0.14.5",
  "io.circe" %% "circe-generic-extras" % "0.14.3",
  "io.circe" %% "circe-parser" % "0.14.5",
)

//resolvers ++= Resolver.sonatypeOssRepos("snapshots")
//resolvers ++= Resolver.sonatypeOssRepos("public")

lazy val root = (project in file("."))
  .settings(
    name := "MoMo"
  )
