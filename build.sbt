ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.10",
  "org.scala-lang" % "scala-compiler" % "2.13.10",
  "org.scalactic" %% "scalactic" % "3.2.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "MoMo"
  )
