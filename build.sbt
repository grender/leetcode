import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.grender.leetcode"
ThisBuild / organizationName := "leetcode"

lazy val root = (project in file("."))
  .settings(
    name := "leetcode",
    libraryDependencies ++= Seq(
      munit
    ),
    scalacOptions ++= Seq("-Ymacro-annotations", "-Ywarn-unused"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafmtOnCompile := true,
    scalafixOnCompile := true
  )


  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  
  testFrameworks += new TestFramework("munit.Framework")


// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
