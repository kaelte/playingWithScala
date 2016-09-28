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

lazy val welcomeMessage = taskKey[Unit]("An example task")

lazy val commonSettings = Seq(
  welcomeMessage := { println("!!! Welcome to the Infinite !!!") },
  cleanKeepFiles <+= target { target => target/ "scala-2.11" },
  fork := true,
  mainClass in (Compile,run) := Some("ordinals.ordinals"),
  name := "Ordinals",
  scalaVersion := "2.11.8",
  version := "0.1.0"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)
