scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps",
  "-optimise",
  "-unchecked",
  "-Yno-generic-signatures",
  "-Yno-adapted-args",
  "-Yinline", "-Yinline-warnings",
  "-Ywarn-value-discard"
)

lazy val utf8test = taskKey[Unit]("An example task")

lazy val commonSettings = Seq(
  utf8test := { println("utf8test: α β γ ω ∀ ∃ ∫ ") },
  cleanKeepFiles <+= target { target => target/ "scala-2.11" },
  fork := true,
  mainClass in (Compile,run) := Some("ordinals.ordinalApp"),
  name := "Ordinals",
  scalaVersion := "2.11.8",
  version := "0.1.0"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)



/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
