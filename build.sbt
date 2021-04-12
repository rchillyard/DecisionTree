organization := "com.phasmidsoftware"

name := "DecisionTree"

version := "1.0.5-SNAPSHOT"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.1"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)
