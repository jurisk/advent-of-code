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
    ),
    scalaVersion := "3.7.3",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.6.3",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.7.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
  )
