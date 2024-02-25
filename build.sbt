ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

compile / run / fork := true
run / connectInput := true


libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.10",
  "org.scala-lang" % "scala-compiler" % "2.13.10",
  "org.scalactic" %% "scalactic" % "3.2.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % "test",
  //"ch.qos.logback" % "logback-core" % "1.4.14",
  //"ch.qos.logback" % "logback-classic" % "1.4.7",
  "io.circe" %% "circe-yaml" % "0.14.2",
  "io.circe" %% "circe-core" % "0.14.5",
  "io.circe" %% "circe-generic" % "0.14.5",
  "io.circe" %% "circe-generic-extras" % "0.14.3",
  "io.circe" %% "circe-parser" % "0.14.5",
  "org.scala-lang" %% "toolkit" % "0.1.7",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  //"org.rogach" %% "scallop" % "5.0.1",
  "com.github.scopt" %% "scopt" % "4.1.0",
  "org.typelevel" %% "cats-effect" % "3.5.2" withSources() withJavadoc(),
  //"org.typelevel" %% "log4cats-core"    % "2.6.0",
  //"org.typelevel" %% "log4cats-slf4j"   % "2.6.0"
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)

//assembly / assemblyMergeStrategy := {
//  case PathList("META-INF", _*) => MergeStrategy.discard
//  case _                        => MergeStrategy.first
//}

lazy val root = (project in file("."))
  .settings(
    name := "MoMo"
  )
