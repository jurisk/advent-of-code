ThisBuild / version := "1.0"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    ),
  )

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
