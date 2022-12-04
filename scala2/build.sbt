ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2022",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.14" // Not "test" on purpose as it is more convenient to keep everything together
    )
  )
