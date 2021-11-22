organization := "com.phasmidsoftware"

name := "DecisionTree"

version := "1.0.5-SNAPSHOT"

scalaVersion := "2.13.5"

val scalaTestVersion = "3.2.9"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog" % "1.0.8",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.7" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "junit" % "junit" % "4.13.2" % "test"
)
