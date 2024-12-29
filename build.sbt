ThisBuild / scalaVersion := "3.3.4"

lazy val core = (projectMatrix in file("core"))
  .settings(
    name := "core"
  )
  .jvmPlatform(scalaVersions = Seq("3.3.4"))
  .jsPlatform(scalaVersions = Seq("3.3.4"))
  .nativePlatform(scalaVersions = Seq("3.3.4"))
