seq(assemblySettings : _*)

organization := "io.bibimbap"

name := "bibimbap"

version := "0.2.0"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

//fork := true

//javaOptions in (Test, run) += "-Djline.shutdownhook=false"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "org.scalaj" %% "scalaj-http" % "1.1.5",
    "org.jsoup" % "jsoup" % "1.8.3",
    "com.typesafe.play" %% "play-json" % "2.4.5",
    "com.typesafe.play" %% "play-ws" % "2.4.5",
    "com.typesafe.akka" %% "akka-actor" % "2.3.8",
    "jline" % "jline" % "2.12",
    "org.apache.lucene" % "lucene-core" % "3.6.0",
    "commons-io" % "commons-io" % "2.4",
    "org.apache.commons" % "commons-lang3" % "3.1"
)

mainClass in (Compile, run) := Some("io.bibimbap.Main")
