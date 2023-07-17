ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala2",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "org.typelevel" %% "mouse" % "1.2.1",
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "co.fs2" %% "fs2-core" % "3.7.0",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.16" // Not "test" on purpose as it is more convenient to keep everything together
    ),
    tpolecatScalacOptions ~= { opts =>
      opts.filterNot(Set(
        ScalacOptions.warnValueDiscard,
        ScalacOptions.warnUnusedPrivates,
        ScalacOptions.warnNumericWiden,
        ScalacOptions.warnUnusedImports,
      ))
    }
  )

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
