name := "resilient-try"

version := "0.1"

scalaVersion := "2.13.4"

val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.2"
val ScalaTestVersion = "3.2.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % "test"
)
