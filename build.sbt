name := "csv-inverter-parser"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("releases")

val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "0.10.4",
  "co.fs2" %% "fs2-io" % "0.10.4",

  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "com.amazonaws" % "aws-java-sdk" % "1.11.368",

  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")