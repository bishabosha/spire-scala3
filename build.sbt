val dottyVersion = "0.16.0-RC3"
// val dottyVersion = dottyLatestNightlyBuild.get

lazy val macros =
  project
    .in(file("macros"))
    .settings(
      name := "spire-scala3-macros",
      version := "0.1.0-SNAPSHOT",

      scalaVersion := dottyVersion,

      libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
    )

lazy val root = project
  .in(file("."))
  .settings(
    name := "spire-scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0" % "test").withDottyCompat(scalaVersion.value)
  )
  .dependsOn(macros)
