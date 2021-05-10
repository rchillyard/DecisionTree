organization := "com.phasmidsoftware"

name := "DecisionTree"

version := "1.0.5-SNAPSHOT"

scalaVersion := "2.13.5"

javacOptions ++= Seq("-source", "1.14", "-target", "1.14")

val scalaTestVersion = "3.2.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-target:jvm-1.14")

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)
