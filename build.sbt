organization := "org.caoilte"
name := "caoilte-snap-kata"

version := "1.0"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.typelevel" %% "cats-free" % "2.1.1",
  "org.typelevel" %% "cats-effect" % "2.1.2",
  "org.typelevel" %% "cats-mtl-core" % "0.7.0",
  "org.typelevel" %% "cats-tagless-core" % "0.11",
  "org.typelevel" %% "cats-tagless-macros" % "0.11",
  "co.fs2" %% "fs2-core" % "2.2.2",
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
