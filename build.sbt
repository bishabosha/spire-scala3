val dottyVersion = "0.16.0-RC3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "spire-scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
