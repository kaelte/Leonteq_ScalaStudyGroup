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
  welcomeMessage := { println("!!! Welcome to Leonteq Study Group !!!") },
  cleanKeepFiles <+= target { target => target },
  fork := true,
  mainClass in (Compile,run) := Some("fp_nith.nith_Chapter_02"),
  name := "Leonteq Study Group",
  scalaVersion := "2.12.1",
  version := "0.2.0"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)
