val dottyVersion = "0.20.0-RC1"
// val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "spire-scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0" % "test").withDottyCompat(scalaVersion.value)
  )
