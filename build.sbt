lazy val osName: String = sys.props("os.name")
val characterEncoding: String = if (osName.matches(".*indows.*")) "Cp1252" else "UTF8"

scalacOptions ++= Seq(
  "-deprecation"
    ,"-encoding", characterEncoding
    ,"-feature"
    ,"-language:implicitConversions"
    ,"-language:higherKinds"
    ,"-language:postfixOps"
    ,"-unchecked"
)

scalaVersion in ThisBuild := "2.11.8"

lazy val utf8test = taskKey[Unit]("An example task")

lazy val commonSettings = Seq(
  fork := true
    ,scalaVersion := "2.11.8"
    ,version := "0.1.1"
    //  ,resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

lazy val specificSettings = Seq(
  mainClass in (Compile,run) := Some("ordinals.ordinalApp")
    ,name := "Ordinals"
    ,utf8test := {
    println("osName = "+osName)
    println("characterEncoding = "+characterEncoding)
    println("utf8test: α β γ ω ∀ ∃ ∫ ")
  }
)

lazy val root = (project in file(".")).settings(commonSettings: _*).settings(specificSettings: _*)



/// Local Variables:
/// mode: scala
/// coding: utf-8
/// End:
