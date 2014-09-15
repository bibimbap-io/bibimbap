seq(assemblySettings : _*)

organization := "io.bibimbap"

name := "bibimbap"

version := "0.2.0"

scalaVersion := "2.11.2"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

//fork := true

//javaOptions in (Test, run) += "-Djline.shutdownhook=false"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "com.typesafe.akka" %% "akka-actor" % "2.3.4",
    "jline" % "jline" % "2.12",
    "org.apache.lucene" % "lucene-core" % "3.6.0",
    "commons-io" % "commons-io" % "2.4",
    "org.apache.commons" % "commons-lang3" % "3.1"
)

mainClass in (Compile, run) := Some("io.bibimbap.Main")
