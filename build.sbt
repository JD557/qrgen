ThisBuild / organization := "eu.joaocosta"
ThisBuild / publishTo    := None //sonatypePublishToBundle.value
ThisBuild / scalaVersion := "3.3.4"
ThisBuild / licenses     := Seq("MIT License" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / homepage     := Some(url("https://github.com/JD557/qegen"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/JD557/qrgen"),
    "scm:git@github.com:JD557/qrgen.git"
  )
)
ThisBuild / versionScheme   := Some("semver-spec")
ThisBuild / autoAPIMappings := true
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-unchecked"
)

// Don't publish the root project
publish / skip  := true
publish         := (())
publishLocal    := (())
publishArtifact := false
publishTo       := None

lazy val core =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(
      name := "qrgen",
    )

lazy val baselineTests =
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("baseline"))
    .dependsOn(core)
    .settings(
      name := "qrgen-baseline-tests",
      libraryDependencies ++= Seq(
        "io.nayuki" % "qrcodegen" % "1.8.0",
        "org.scalameta" %%% "munit" % "1.0.3" % Test,
        "org.scalameta" %%% "munit-scalacheck" % "1.0.0" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      publish / skip  := true,
      publish         := (()),
      publishLocal    := (()),
      publishArtifact := false,
      publishTo       := None
    )
