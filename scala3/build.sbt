lazy val root = project
  .in(file(""))
  .settings(
    name := "advent-of-code-scala3",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Xkind-projector",
      "-source", "3.3"
    ),
    scalaVersion := "3.5.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "org.scalameta" %% "munit" % "1.0.0" % Test,
    ),
  )
