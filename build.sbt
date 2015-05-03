seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in (Compile,run) := Some("nith_Chapter_07")

fork := true

cleanKeepFiles <+= target { target => target/ "scala-2.10" }

