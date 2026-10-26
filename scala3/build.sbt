lazy val root = project
  .in(file(""))
  .settings(
    name := "advent-of-code-scala3",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Xkind-projector",
      "-source", "3.3",
      "-deprecation",
      "-Wunused:imports",
    ),
    scalaVersion := "3.9.0",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.7.1",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.8.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    ),
    scalafixDependencies ++= List(
      "org.typelevel"       %% "typelevel-scalafix" % "0.6.0",
    ),
    // Scalafix settings
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    // Scalafmt settings
    scalafmtOnCompile := true,
  )
