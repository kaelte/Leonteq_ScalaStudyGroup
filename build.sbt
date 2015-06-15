seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

scalacOptions     ++= Seq(
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

mainClass in (Compile,run) := Some("fp_nith.nith_Chapter_09")

fork := true

cleanKeepFiles <+= target { target => target/ "scala-2.10" }
