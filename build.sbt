val dottyVersion = "0.26.0-RC1"
// val dottyVersion = dottyLatestNightlyBuild.get
val spireScala3Version = "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "spire-scala3",
    version := spireScala3Version,

    scalaVersion := dottyVersion,
    scalacOptions += "-Yexplicit-nulls",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0" % Test).withDottyCompat(scalaVersion.value)
  )
