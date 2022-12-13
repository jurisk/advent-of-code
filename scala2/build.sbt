ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala2",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "mouse" % "1.2.1",
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "org.scalatest" %% "scalatest" % "3.2.14" // Not "test" on purpose as it is more convenient to keep everything together
    )
  )

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
